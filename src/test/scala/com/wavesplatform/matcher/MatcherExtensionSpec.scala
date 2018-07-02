package com.wavesplatform.matcher

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.assets.exchange.AssetPair
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.utils.Base58
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class MatcherExtensionSpec extends FreeSpec with Matchers {
  private val WAVES  = "WAVES"
  private val WUSD   = "HyFJ3rrq5m7FxdkWtQXkZrDat1F7LjVVGfpSkUuEXQHj"
  private val WBTC   = "Fmg13HEHJHuZYbtJq8Da8wifJENq8uBxDuWoP9pVe2Qe"
  private val WEUR   = "2xnE3EdpqXtFgCP156qt1AbyjpqdZ5jGjWo3CwTawcux"
  private val WCNY   = "6pmDivReTLikwYqQtJTv6dTcE59knriaodB3AK8T9cF8"
  private val Asset1 = Base58.encode(Array.fill[Byte](32)(1))
  private val Asset2 = Base58.encode(Array.fill[Byte](32)(2))

  private val priceAssets = ConfigFactory.parseString(s"waves.matcher.price-assets = [$WAVES,$WBTC,$WUSD,$WEUR,$WCNY]")

  def withActorSystem(overrides: Config = priceAssets)(f: ActorSystem => Unit): Unit = {
    val system = ActorSystem("test-system", loadConfig(overrides))
    f(system)
    Await.result(system.terminate(), Duration.Inf)
  }

  val pairs = Table(
    ("amount", "price", "valid"),
    (WAVES, WUSD, false),
    (WUSD, WAVES, true),
    (WBTC, WEUR, false),
    (WEUR, WBTC, true),
    (Asset1, WAVES, true),
    (WAVES, Asset1, false),
    (Asset2, Asset1, true),
    (Asset1, Asset2, false),
  )

  "MatcherExtension" - {
    "correctly orders pairs when" in withActorSystem() { s =>
      forAll(pairs) { (amountAsset, priceAsset, isValid) =>
        val pair = AssetPair.createAssetPair(amountAsset, priceAsset).get
        MatcherExtension(s).isValidPair(pair) shouldBe isValid
      }
    }
  }
}
