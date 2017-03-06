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
    val isTrue = offerGroups.map(b => {
      val head = basketGroups.filter(_._1 == b._1).headOption.getOrElse(null)
      if (b._2 <= head._2) true else false
    })
    !isTrue.contains(false)
  }

  private def applySpecialOfferApply(offer: OfferItem) = {
    val basketGroups = basket.groupBy(_.name).mapValues(_.size).toSeq
    val offerGroups = offer.products.groupBy(_.name).mapValues(_.size).toSeq
    val noOfSameOffer = offerGroups.map(b => {
      val head = basketGroups.filter(_._1 == b._1).head
      head._2 / b._2
    })
    for (i <- 1 to noOfSameOffer.max) yield offer
  }

  def totalCostOfBasketWithDiscount = applySpecialOffersToBasket(validSpecialOffers)
}
