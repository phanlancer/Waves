package com.wavesplatform.it.async

import com.wavesplatform.crypto
import com.wavesplatform.it.Node
import com.wavesplatform.it.async.matcher.OrderExclusionTestSuite.MatcherFee
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._

package object matcher {
  def prepareOrder(node: Node,
                   matcherNode: Node,
                   pair: AssetPair,
                   orderType: OrderType,
                   price: Long,
                   amount: Long,
                   timeToLive: Duration = 30.days - 1.seconds,
                   creationTime: Long = System.currentTimeMillis()): Order = {
    val timeToLiveTimestamp = creationTime + timeToLive.toMillis
    val matcherPublicKey    = matcherNode.publicKey
    val unsigned            = Order(node.publicKey, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLiveTimestamp, MatcherFee, Array())
    val signature           = crypto.sign(node.privateKey, unsigned.toSign)
    unsigned.copy(signature = signature)
  }
}
