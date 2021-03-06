package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scorex.account.PublicKeyAccount
import scorex.transaction.assets.{ReissueTransaction, ReissueTransactionV1}

class ReissueTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Reissue serialization roundtrip") {
    forAll(reissueGen) { issue: ReissueTransaction =>
      val recovered = issue.builder.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Reissue serialization from TypedTransaction") {
    forAll(reissueGen) { issue: ReissueTransaction =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                    "type": 5,
                    "id": "2y8pNQteNQnY5JWtrZGLUv3tD6GFT6DDzBWttVTwBa2t",
                    "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                    "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                    "fee": 100000000,
                    "timestamp": 1526287561757,
                    "signature": "3LnRMrjkk7RoV35PTwcdB4yW2rqUqXaKAh8DnPk5tNWABvhVQ9oqdTk3zM8b9AbGtry7WEcQZtevfK92DCFaa6hA",
                    "version": 1,
                    "chainId": null,
                    "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                    "quantity": 100000000,
                    "reissuable": true
                    }
    """)

    val tx = ReissueTransactionV1
      .create(
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        100000000L,
        true,
        100000000L,
        1526287561757L,
        ByteStr.decodeBase58("3LnRMrjkk7RoV35PTwcdB4yW2rqUqXaKAh8DnPk5tNWABvhVQ9oqdTk3zM8b9AbGtry7WEcQZtevfK92DCFaa6hA").get
      )
      .right
      .get

    js shouldEqual tx.json()
  }

}
