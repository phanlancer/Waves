package com.wavesplatform.matcher

import com.google.common.base.Charsets.UTF_8
import com.wavesplatform.state.{Blockchain, ByteStr}
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.AssetPair
import scorex.utils.ByteArray

class MatcherExtension(settings: MatcherSettings, blockchain: Blockchain) {
  import MatcherExtension._

  import Either.cond
  import Ordered._

  private[this] val indices             = settings.priceAssets.zipWithIndex.toMap
  private[this] val predefinedPairs     = settings.predefinedPairs.toSet
  private[this] val blacklistedAssetIds = settings.blacklistedAssets

  def isValidPair(pair: AssetPair): Boolean =
    predefinedPairs.contains(pair) || ((indices.get(pair.priceAssetStr), indices.get(pair.amountAssetStr)) match {
      case (None, None)         => pair.priceAsset < pair.amountAsset
      case (Some(_), None)      => true
      case (None, Some(_))      => false
      case (Some(pi), Some(ai)) => pi < ai
    })

  private def isValidAssetId(assetId: ByteStr): Boolean = blockchain.assetDescription(assetId).exists { d =>
    settings.blacklistedNames.forall(_.findFirstIn(new String(d.description, UTF_8)).isEmpty)
  }

  private def validateAssetId(assetId: String): Either[String, Option[AssetId]] =
    for {
      aid <- AssetPair.extractAssetId(assetId).toEither.left.map(_ => errorMsg(assetId))
      _   <- cond(aid.forall(isValidAssetId) && !blacklistedAssetIds(assetId), aid, errorMsg(assetId))
    } yield aid

  def createAssetPair(a1: String, a2: String): Either[String, AssetPair] =
    for {
      amount <- validateAssetId(a1)
      price  <- validateAssetId(a2)
      pair = AssetPair(amount, price)
      validPair <- cond(isValidPair(pair), pair, "Pair should be reverse")
    } yield validPair
}

object MatcherExtension {
  private def errorMsg(assetId: String) = s"Invalid Asset ID: $assetId"

  implicit val assetIdOrdering: Ordering[Option[ByteStr]] = (buffer1: Option[ByteStr], buffer2: Option[ByteStr]) =>
    (buffer1, buffer2) match {
      case (None, None)                           => 0
      case (_, None)                              => 1
      case (None, _)                              => -1
      case (Some(ByteStr(b1)), Some(ByteStr(b2))) => ByteArray.compare(b1, b2)
  }
}
