package Offers

import Products.ProductItem

/**
  * Created by Nenyi on 06/03/2017.
  */
class OfferItem(offerDescription: String, offerProducts: List[ProductItem], total: Double) {
  val description = offerDescription
  val products = offerProducts
  val discountTotal = total
}