import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.BeforeAndAfter
import Products._
import Offers._


/**
  * Created by Nenyi on 06/03/2017.
  */
class SpecialOffersTest extends FlatSpec with BeforeAndAfter with SpecialOffers {
  "SpecialOffers" should "have a Buy One Get One Free Special Offer" in {
    SpecialOffers(BuyOneGetOneFree) shouldBe true
  }
  it should "have a Three For The Price Of Two Special Offer" in {
    SpecialOffers(ThreeForTwo) shouldBe true
  }

  "Buy One Get One Free" should "be for two apples" in {
    BuyOneGetOneFree.products shouldBe List(Apple, Apple)
  }
  it should "have a total price of £0.60" in {
    BuyOneGetOneFree.discountTotal shouldBe 0.60
  }

  "Three For The Price Of Two" should "be for three Oranges" in {
    ThreeForTwo.products shouldBe List(Orange, Orange, Orange)
  }
  it should "have a total price of £0.50" in {
    ThreeForTwo.discountTotal shouldBe 0.50
  }
}
