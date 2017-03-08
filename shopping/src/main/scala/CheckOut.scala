import Offers.OfferItem
import Products.ProductItem
/**
  * Created by Nenyi on 06/03/2017.
  */
class CheckOut(shoppingBasket: List[ProductItem]) extends Inventory with SpecialOffers {
  val basket = shoppingBasket

  def totalCostOfBasket = basket.map(_.price).sum

  def validSpecialOffers = (for (s <- SpecialOffers if (applySpecialOfferFunction[Boolean, Boolean](s, doesOfferApplyList, isOfferApplicable))) yield applySpecialOfferFunction[Int, IndexedSeq[Int]](s, offerApply, sameOffersList)).toList.flatten.asInstanceOf[List[OfferItem]]

  private def applySpecialOffersToBasket(validSpecialOffers: List[OfferItem]) = {
    val validSpecialOffersTotal = validSpecialOffers.map(v => {
      v.products.map(_.price).sum - v.discountTotal
    }).sum
    BigDecimal(totalCostOfBasket - validSpecialOffersTotal).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  private def extractProductGroupings(list: List[ProductItem]) = list.groupBy(_.name).mapValues(_.size).toSeq

  private def applySpecialOfferFunction[T, S](offer: OfferItem, offerFunction: (Seq[(String, Int)], Seq[(String, Int)]) => Seq[Any], applyFunction: (OfferItem, Seq[T]) => Any) = {
    val basketGroups = extractProductGroupings(basket)
    val offerGroups = extractProductGroupings(offer.products)
    val offerResult = offerFunction(offerGroups, basketGroups).asInstanceOf[Seq[T]]
    applyFunction(offer, offerResult).asInstanceOf[S]
  }

  private val isOfferApplicable = (offer: OfferItem, validProducts: Seq[Boolean]) => !validProducts.contains(false)
  private val sameOffersList = (offer: OfferItem, noOfSameOffer: Seq[Int]) => for (i <- 1 to noOfSameOffer.min) yield offer
  private val doesOfferApplyList = (offerGroups: Seq[(String, Int)], basketGroups: Seq[(String, Int)]) => {
    offerGroups.map(item => {
      doesOfferApply(item._1, item._2, basketGroups)
    })
  }

  private val doesOfferApply = (name: String, quantity: Int, basketGroups: Seq[(String, Int)]) => {
    val basketProductGroup = basketGroups.filter(_._1 == name).head
    val (_, productQuantity) = basketGroups.filter(_._1 == name).headOption match {
      case None => (None, 0)
      case Some(productGroup) => (productGroup, productGroup._2)
    }
    if (quantity <= productQuantity) true else false
  }

  private val offerApply = (offerGroups: Seq[(String, Int)], basketGroups: Seq[(String, Int)]) => {
    offerGroups.map { case (item: String, numberOfOffers: Int) => {
      val head = basketGroups.filter(_._1 == item).head
      head._2 / numberOfOffers
    }}
  }

  def totalCostOfBasketWithDiscount = applySpecialOffersToBasket(validSpecialOffers)
}
