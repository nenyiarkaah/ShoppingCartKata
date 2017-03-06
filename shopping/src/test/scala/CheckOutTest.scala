import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.BeforeAndAfter
import Products._


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
}
