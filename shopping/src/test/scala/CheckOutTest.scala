import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.BeforeAndAfter
import Products._
import Offers._

/**
  * Created by Nenyi on 06/03/2017.
  */
class CheckOutTest extends FlatSpec with BeforeAndAfter {
  var basket: List[ProductItem] = _
  var checkOut: CheckOut = _
  before {
    basket = List[ProductItem](); checkOut = new CheckOut(basket)
  }

  "Checkout" should "have Apples" in {
    checkOut.products(Apple) shouldBe true
  }
  it should "have Oranges" in {
    checkOut.products(Orange) shouldBe true
  }
  it should "have Buy One Get One Free Offer" in {
    checkOut.SpecialOffers(BuyOneGetOneFree) shouldBe true
  }
  it should "have Three For Two Special Offer" in {
    checkOut.SpecialOffers(ThreeForTwo) shouldBe true
  }

  "Apple" should "have the name Apple" in {
    Apple.name shouldBe "Apple"
  }
  it should "have a price of £0.60" in {
    Apple.price shouldBe 0.60
  }

  "Orange" should "have the name Orange" in {
    Orange.name shouldBe "Orange"
  }
  it should "have a price of £0.25" in {
    Orange.price shouldBe 0.25
  }

  "Shopping Basket" should "be a list of products" in {
    val basket = List[ProductItem]()
    checkOut.basket shouldBe basket
  }
  it should "be able to hold a list of products" in {

    checkOut = new CheckOut(basket)
    checkOut.basket shouldBe basket
  }
  it should "output the total cost of a basket" in {
    basket = List[ProductItem](Apple, Apple, Orange, Apple)
    checkOut = new CheckOut(basket)
    checkOut.totalCostOfBasket shouldBe 2.05
  }

  "Special Offers" should "find which Special Offers apply to the basket" in {
    basket = List[ProductItem](Apple, Apple, Orange, Apple)
    checkOut = new CheckOut(basket)
    checkOut.validSpecialOffers shouldBe List(BuyOneGetOneFree)
  }
  it should "be applied to the Total cost" in {
    basket = List[ProductItem](Apple, Apple, Orange, Apple)
    checkOut = new CheckOut(basket)
    checkOut.totalCostOfBasketWithDiscount shouldBe 1.45
  }
  it should "find 2 BuyOneGetOneFree" in {
    basket = List[ProductItem](Apple, Apple, Orange, Apple, Apple)
    checkOut = new CheckOut(basket)
    checkOut.validSpecialOffers shouldBe List(BuyOneGetOneFree, BuyOneGetOneFree)
  }
  it should "find 2 BuyOneGetOneFree and 1 ThreeForTwo" in {
    basket = List[ProductItem](Apple, Apple, Orange, Apple, Apple, Orange, Orange)
    checkOut = new CheckOut(basket)
    checkOut.validSpecialOffers shouldBe List(BuyOneGetOneFree, BuyOneGetOneFree, ThreeForTwo)
  }
  it should "2 BuyOneGetOneFree and 1 ThreeForTwo be applied to the Total cost" in {
    basket = List[ProductItem](Apple, Apple, Orange, Apple, Apple, Orange, Orange)
    checkOut = new CheckOut(basket)
    checkOut.totalCostOfBasketWithDiscount shouldBe 1.70
  }
}
