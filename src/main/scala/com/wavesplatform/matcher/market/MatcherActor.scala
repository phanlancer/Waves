package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.persistence.{PersistentActor, RecoveryCompleted}
import akka.routing.FromConfig
import com.google.common.base.Charsets
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{MatcherResponse, StatusCodeMatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.matcher.model.OrderBook
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils.Base58
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import play.api.libs.json._
import scorex.account.Address
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils._
import scorex.wallet.Wallet

import scala.collection.{immutable, mutable}

class MatcherActor(orderHistory: ActorRef,
                   updateSnapshot: AssetPair => OrderBook => Unit,
                   wallet: Wallet,
                   utx: UtxPool,
                   allChannels: ChannelGroup,
                   settings: MatcherSettings,
                   blockchain: Blockchain,
                   functionalitySettings: FunctionalitySettings)
    extends PersistentActor
    with ScorexLogging {

  import MatcherActor._

  val tradedPairs = mutable.Map.empty[AssetPair, MarketData]

  def getAssetName(asset: Option[AssetId]): String =
    asset.fold(AssetPair.WavesName) { aid =>
      blockchain.assetDescription(aid).fold("Unknown")(d => new String(d.name, Charsets.UTF_8))
    }

  def createOrderBook(pair: AssetPair): ActorRef = {
    val md = MarketData(
      pair,
      getAssetName(pair.amountAsset),
      getAssetName(pair.priceAsset),
      NTP.correctedTime(),
      pair.amountAsset.flatMap(blockchain.assetDescription).map(t => AssetInfo(t.decimals)),
      pair.priceAsset.flatMap(blockchain.assetDescription).map(t => AssetInfo(t.decimals))
    )
    tradedPairs += pair -> md

    context.actorOf(
      OrderBookActor.props(pair, updateSnapshot(pair), orderHistory, blockchain, settings, wallet, utx, allChannels, functionalitySettings),
      OrderBookActor.name(pair)
    )
  }

  def checkBlacklistedAddress(address: Address)(f: => Unit): Unit = {
    val v = !settings.blacklistedAddresses.contains(address.address) :| s"Invalid Address: ${address.address}"
    if (!v) {
      sender() ! StatusCodeMatcherResponse(StatusCodes.Forbidden, v.messages())
    } else {
      f
    }
  }

  def createAndForward(order: Order): Unit = {
    val orderBook = createOrderBook(order.assetPair)
    persistAsync(OrderBookCreated(order.assetPair)) { _ =>
      forwardReq(order)(orderBook)
    }
  }

  def returnEmptyOrderBook(pair: AssetPair): Unit = {
    sender() ! GetOrderBookResponse.empty(pair)
  }

  def forwardReq(req: Any)(orderBook: ActorRef): Unit = orderBook forward req

  def getMatcherPublicKey: Array[Byte] = {
    wallet.findPrivateKey(settings.account).map(_.publicKey).getOrElse(Array())
  }

  def forwardToOrderBook: Receive = {
    case GetMarkets =>
      sender() ! GetMarketsResponse(getMatcherPublicKey, tradedPairs.values.toSeq)

    case order: Order =>
      checkBlacklistedAddress(order.senderPublicKey) {
        context
          .child(OrderBookActor.name(order.assetPair))
          .fold(createAndForward(order))(forwardReq(order))
      }

    case ob: DeleteOrderBookRequest =>
      context
        .child(OrderBookActor.name(ob.assetPair))
        .fold(returnEmptyOrderBook(ob.assetPair))(forwardReq(ob))
      removeOrderBook(ob.assetPair)

    case x: CancelOrder =>
      context
        .child(OrderBookActor.name(x.assetPair))
        .fold {
          sender() ! OrderCancelRejected(s"Order '${x.orderId}' is already cancelled or never existed in '${x.assetPair.key}' pair")
        }(forwardReq(x))

    case x: ForceCancelOrder =>
      context
        .child(OrderBookActor.name(x.assetPair))
        .fold {
          sender() ! OrderCancelRejected(s"Order '${x.orderId}' is already cancelled or never existed in '${x.assetPair.key}' pair")
        }(forwardReq(x))

    case ob: OrderBookRequest =>
      context
        .child(OrderBookActor.name(ob.assetPair))
        .fold(returnEmptyOrderBook(ob.assetPair))(forwardReq(ob))
  }

  def initPredefinedPairs(): Unit = {
    settings.predefinedPairs.diff(tradedPairs.keys.toSeq).foreach(pair => createOrderBook(pair))
  }

  private def removeOrderBook(pair: AssetPair): Unit = {
    if (tradedPairs.contains(pair)) {
      tradedPairs -= pair
      deleteMessages(lastSequenceNr)
      persistAll(tradedPairs.map(v => OrderBookCreated(v._1)).to[immutable.Seq]) { _ =>
        }
    }
  }

  override def receiveRecover: Receive = {
    case OrderBookCreated(pair) =>
      context
        .child(OrderBookActor.name(pair))
        .getOrElse(createOrderBook(pair))
    case RecoveryCompleted =>
      log.info("MatcherActor - Recovery completed!")
      initPredefinedPairs()
      createBalanceWatcher()
  }

  override def receiveCommand: Receive = forwardToOrderBook

  override def persistenceId: String = "matcher"

  private def createBalanceWatcher(): Unit = if (settings.balanceWatching.enable) {
    val balanceWatcherMaster =
      context.actorOf(FromConfig.props(BalanceWatcherWorkerActor.props(settings.balanceWatching, self, orderHistory)), "balance-watcher-router")
    context.system.eventStream.subscribe(balanceWatcherMaster, classOf[BalanceChanged])
  }
}

object MatcherActor {
  def name = "matcher"

  def props(orderHistoryActor: ActorRef,
            updateSnapshot: AssetPair => OrderBook => Unit,
            wallet: Wallet,
            utx: UtxPool,
            allChannels: ChannelGroup,
            settings: MatcherSettings,
            blockchain: Blockchain,
            functionalitySettings: FunctionalitySettings): Props =
    Props(new MatcherActor(orderHistoryActor, updateSnapshot, wallet, utx, allChannels, settings, blockchain, functionalitySettings))

  case class OrderBookCreated(pair: AssetPair)

  case object GetMarkets

  case class GetMarketsResponse(publicKey: Array[Byte], markets: Seq[MarketData]) extends MatcherResponse {
    def getMarketsJs: JsValue =
      JsArray(
        markets.map(m =>
          Json.obj(
            "amountAsset"     -> m.pair.amountAssetStr,
            "amountAssetName" -> m.amountAssetName,
            "amountAssetInfo" -> m.amountAssetInfo,
            "priceAsset"      -> m.pair.priceAssetStr,
            "priceAssetName"  -> m.priceAssetName,
            "priceAssetInfo"  -> m.priceAssetinfo,
            "created"         -> m.created
        )))

    def json: JsValue = Json.obj(
      "matcherPublicKey" -> Base58.encode(publicKey),
      "markets"          -> getMarketsJs
    )

    def code: StatusCode = StatusCodes.OK
  }

  case class AssetInfo(decimals: Int)
  implicit val assetInfoFormat: Format[AssetInfo] = Json.format[AssetInfo]

  case class MarketData(pair: AssetPair,
                        amountAssetName: String,
                        priceAssetName: String,
                        created: Long,
                        amountAssetInfo: Option[AssetInfo],
                        priceAssetinfo: Option[AssetInfo])

  def compare(buffer1: Option[Array[Byte]], buffer2: Option[Array[Byte]]): Int = {
    if (buffer1.isEmpty && buffer2.isEmpty) 0
    else if (buffer1.isEmpty) -1
    else if (buffer2.isEmpty) 1
    else ByteArray.compare(buffer1.get, buffer2.get)
  }
}
