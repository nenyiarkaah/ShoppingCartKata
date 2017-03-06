import Offers.OfferItem
import Products.ProductItem
/**
  * Created by Nenyi on 06/03/2017.
  */
class CheckOut(shoppingBasket: List[ProductItem]) extends Inventory with SpecialOffers {
  val basket = shoppingBasket

  def totalCostOfBasket = basket.map(_.price).sum

  def validSpecialOffers = (for (s <- SpecialOffers if (doesSpecialOfferApply(s))) yield applySpecialOfferApply(s)).toList.flatten

  private def applySpecialOffersToBasket(validSpecialOffers: List[OfferItem]) = {
    val validSpecialOffersTotal = validSpecialOffers.map(v => {
      v.products.map(_.price).sum - v.discountTotal
    }).sum
    BigDecimal(totalCostOfBasket - validSpecialOffersTotal).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  private def doesSpecialOfferApply(offer: OfferItem) = {
    val basketGroups = basket.groupBy(_.name).mapValues(_.size).toSeq
    val offerGroups = offer.products.groupBy(_.name).mapValues(_.size).toSeq
    val isTrue = offerGroups.map(item => {
      val basketProductGroup = basketGroups.filter(_._1 == item._1).head
      val (product, productQuantity) = basketGroups.filter(_._1 == item._1).headOption match {
        case None => (None, 0)
        case Some(x) => (x, x._2)
      }
      if (item._2 <= productQuantity) true else false
    })
    !isTrue.contains(false)
  }

  private def applySpecialOfferApply(offer: OfferItem) = {
    val basketGroups = basket.groupBy(_.name).mapValues(_.size).toSeq
    val offerGroups = offer.products.groupBy(_.name).mapValues(_.size).toSeq
    val noOfSameOffer = offerGroups.map{case (item: String, numberOfItems: Int) => {
      val head = basketGroups.filter(_._1 == item).head
      head._2 / numberOfItems
    }}
    for (i <- 1 to noOfSameOffer.min) yield offer
  }

  def totalCostOfBasketWithDiscount = applySpecialOffersToBasket(validSpecialOffers)
}
