import Products.ProductItem
/**
  * Created by Nenyi on 06/03/2017.
  */
class CheckOut(shoppingBasket: List[ProductItem]) extends Inventory {
  val basket = shoppingBasket

  def totalCostOfBasket = basket.map(_.price).sum

}
