package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import com.wavesplatform.generator.NarrowTransactionGenerator.Settings
import com.wavesplatform.state.DataEntry.{MaxValueSize, Type}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, EitherExt2, IntegerDataEntry}
import org.slf4j.LoggerFactory
import scorex.account.{Alias, PrivateKeyAccount}
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseCancelTransactionV1, LeaseTransactionV1}
import scorex.transaction.transfer.MassTransferTransaction.ParsedTransfer
import scorex.transaction.transfer._
import scorex.utils.LoggerFacade

import scala.concurrent.duration._
import scala.util.Random

class NarrowTransactionGenerator(settings: Settings, val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {

  private def r = ThreadLocalRandom.current

  private val log     = LoggerFacade(LoggerFactory.getLogger(getClass))
  private val typeGen = new DistributedRandomGenerator(settings.probabilities)

  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  private def logOption[T <: Transaction](txE: Either[ValidationError, T])(implicit m: Manifest[T]): Option[T] = {
    txE match {
      case Left(e) =>
        log.warn(s"${m.runtimeClass.getName}: ${e.toString}")
        None
      case Right(tx) => Some(tx)
    }
  }

  override def next(): Iterator[Transaction] = generate(settings.transactions).toIterator

  def generate(n: Int): Seq[Transaction] = {
    val issueTransactionSender = randomFrom(accounts).get
    val tradeAssetIssue = IssueTransactionV1
      .selfSigned(
        issueTransactionSender,
        "TRADE".getBytes,
        "Waves DEX is the best exchange ever".getBytes,
        100000000,
        2,
        reissuable = false,
        100000000L + r.nextInt(100000000),
        System.currentTimeMillis()
      )
      .right
      .get

    val tradeAssetDistribution = {
      tradeAssetIssue +: accounts.map(acc => {
        TransferTransactionV1
          .selfSigned(Some(tradeAssetIssue.id()),
                      issueTransactionSender,
                      acc,
                      5,
                      System.currentTimeMillis(),
                      None,
                      100000,
                      Array.fill(r.nextInt(100))(r.nextInt().toByte))
          .right
          .get
      })
    }

    val generated = (0 until (n * 1.2).toInt).foldLeft(
      (
        Seq.empty[Transaction],
        Seq.empty[IssueTransactionV1],
        Seq.empty[IssueTransactionV1],
        Seq.empty[LeaseTransactionV1],
        Seq.empty[CreateAliasTransaction]
      )) {
      case ((allTxsWithValid, validIssueTxs, reissuableIssueTxs, activeLeaseTransactions, aliases), _) =>
        def moreThatStandartFee = 100000L + r.nextInt(100000)

        def ts = System.currentTimeMillis()

        val tx = typeGen.getRandom match {
          case IssueTransactionV1 =>
            val sender      = randomFrom(accounts).get
            val name        = new Array[Byte](10)
            val description = new Array[Byte](10)
            r.nextBytes(name)
            r.nextBytes(description)
            val reissuable = r.nextBoolean()
            val amount     = 100000000L + Random.nextInt(Int.MaxValue)
            logOption(
              IssueTransactionV1
                .selfSigned(sender, name, description, amount, Random.nextInt(9).toByte, reissuable, 100000000L + r.nextInt(100000000), ts))
          case TransferTransactionV1 =>
            val useAlias  = r.nextBoolean()
            val recipient = if (useAlias && aliases.nonEmpty) randomFrom(aliases).map(_.alias).get else randomFrom(accounts).get.toAddress
            val sendAsset = r.nextBoolean()
            val senderAndAssetOpt = if (sendAsset) {
              val asset = randomFrom(validIssueTxs)
              asset.map(issue => {
                val pk = accounts.find(_ == issue.sender).get
                (pk, Some(issue.id()))
              })
            } else Some((randomFrom(accounts).get, None))
            senderAndAssetOpt.flatMap {
              case (sender, asset) =>
                logOption(
                  TransferTransactionV1
                    .selfSigned(asset,
                                sender,
                                recipient,
                                r.nextInt(500000),
                                ts,
                                None,
                                moreThatStandartFee,
                                Array.fill(r.nextInt(100))(r.nextInt().toByte)))
            }
          case ReissueTransactionV1 =>
            val reissuable = r.nextBoolean()
            randomFrom(reissuableIssueTxs).flatMap(assetTx => {
              val sender = accounts.find(_.address == assetTx.sender.address).get
              logOption(ReissueTransactionV1.selfSigned(sender, assetTx.id(), Random.nextInt(Int.MaxValue), reissuable, moreThatStandartFee, ts))
            })
          case BurnTransactionV1 =>
            randomFrom(validIssueTxs).flatMap(assetTx => {
              val sender = accounts.find(_.address == assetTx.sender.address).get
              logOption(BurnTransactionV1.selfSigned(sender, assetTx.id(), Random.nextInt(1000), moreThatStandartFee, ts))
            })
          case ExchangeTransaction =>
            val matcher   = randomFrom(accounts).get
            val seller    = randomFrom(accounts).get
            val pair      = AssetPair(None, Some(tradeAssetIssue.id()))
            val sellOrder = Order.sell(seller, matcher, pair, 100000000, 1, ts, ts + 30.days.toMillis, moreThatStandartFee * 3)
            val buyer     = randomFrom(accounts).get
            val buyOrder  = Order.buy(buyer, matcher, pair, 100000000, 1, ts, ts + 1.day.toMillis, moreThatStandartFee * 3)
            logOption(ExchangeTransaction.create(matcher, buyOrder, sellOrder, 100000000, 1, 300000, 300000, moreThatStandartFee * 3, ts))
          case LeaseTransactionV1 =>
            val sender   = randomFrom(accounts).get
            val useAlias = r.nextBoolean()
            val recipientOpt =
              if (useAlias && aliases.nonEmpty) randomFrom(aliases.filter(_.sender != sender)).map(_.alias)
              else randomFrom(accounts.filter(_ != sender).map(_.toAddress))
            recipientOpt.flatMap(recipient => logOption(LeaseTransactionV1.selfSigned(sender, 1, moreThatStandartFee * 3, ts, recipient)))
          case LeaseCancelTransactionV1 =>
            randomFrom(activeLeaseTransactions).flatMap(lease => {
              val sender = accounts.find(_.address == lease.sender.address).get
              logOption(LeaseCancelTransactionV1.selfSigned(sender, lease.id(), moreThatStandartFee * 3, ts))
            })
          case CreateAliasTransactionV1 =>
            val sender      = randomFrom(accounts).get
            val aliasString = NarrowTransactionGenerator.generateAlias()
            logOption(CreateAliasTransactionV1.selfSigned(sender, Alias.buildWithCurrentNetworkByte(aliasString).explicitGet(), 100000, ts))
          case MassTransferTransaction =>
            val transferCount = r.nextInt(MassTransferTransaction.MaxTransferCount)
            val transfers = for (i <- 0 to transferCount) yield {
              val useAlias  = r.nextBoolean()
              val recipient = if (useAlias && aliases.nonEmpty) randomFrom(aliases).map(_.alias).get else randomFrom(accounts).get.toAddress
              val amount    = r.nextLong(500000)
              ParsedTransfer(recipient, amount)
            }
            val sendAsset = r.nextBoolean()
            val senderAndAssetOpt = if (sendAsset) {
              val asset = randomFrom(validIssueTxs)
              asset.map(issue => {
                val pk = accounts.find(_ == issue.sender).get
                (pk, Some(issue.id()))
              })
            } else Some((randomFrom(accounts).get, None))
            senderAndAssetOpt.flatMap {
              case (sender, asset) =>
                logOption(
                  MassTransferTransaction.selfSigned(MassTransferTransaction.version,
                                                     asset,
                                                     sender,
                                                     transfers.toList,
                                                     ts,
                                                     100000 + 50000 * transferCount,
                                                     Array.fill(r.nextInt(100))(r.nextInt().toByte)))
            }
          case DataTransaction =>
            val sender = randomFrom(accounts).get
            val count  = r.nextInt(10)

            val data = for {
              _ <- 0 until count
              keyLen = r.nextInt(10)
              key    = Random.nextString(keyLen)
              etype  = r.nextInt(Type.maxId)
            } yield
              etype match {
                case t if t == Type.Integer.id => IntegerDataEntry(key, r.nextLong)
                case t if t == Type.Boolean.id => BooleanDataEntry(key, r.nextBoolean)
                case t if t == Type.Binary.id =>
                  val size = r.nextInt(MaxValueSize + 1)
                  val b    = new Array[Byte](size)
                  r.nextBytes(b)
                  BinaryDataEntry(key, ByteStr(b))
              }
            val size = 128 + data.map(_.toBytes.length).sum
            val fee  = 100000 * (size / 1024 + 1)
            logOption(DataTransaction.selfSigned(1, sender, data.toList, fee, ts))
        }

        (tx.map(tx => allTxsWithValid :+ tx).getOrElse(allTxsWithValid), tx match {
          case Some(tx: IssueTransactionV1) => validIssueTxs :+ tx
          case _                            => validIssueTxs
        }, tx match {
          case Some(tx: IssueTransactionV1) if tx.reissuable  => reissuableIssueTxs :+ tx
          case Some(tx: ReissueTransaction) if !tx.reissuable => reissuableIssueTxs.filter(_.id != tx.id)
          case _                                              => reissuableIssueTxs
        }, tx match {
          case Some(tx: LeaseTransactionV1)     => activeLeaseTransactions :+ tx
          case Some(tx: LeaseCancelTransaction) => activeLeaseTransactions.filter(_.id != tx.leaseId)
          case _                                => activeLeaseTransactions
        }, tx match {
          case Some(tx: CreateAliasTransaction) => aliases :+ tx
          case _                                => aliases
        })
    }

    tradeAssetDistribution ++ generated._1.take(n - tradeAssetDistribution.size)
  }
}

object NarrowTransactionGenerator {

  case class Settings(transactions: Int, probabilities: Map[TransactionParser, Double])

  private val minAliasLength = 4
  private val maxAliasLength = 30
  private val aliasAlphabet  = "-.0123456789@_abcdefghijklmnopqrstuvwxyz".toVector

  def generateAlias(): String = {
    val len = Random.nextInt(maxAliasLength - minAliasLength) + minAliasLength
    Random.shuffle(aliasAlphabet).take(len).mkString
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""transactions per iteration: $transactions
         |probabilities:
         |  ${probabilities.mkString("\n  ")}""".stripMargin
    }
  }

}
