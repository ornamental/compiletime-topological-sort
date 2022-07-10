package org.ornamental.compiletime

import shapeless.::
import shapeless.HList
import shapeless.HNil

/**
 * Type family selecting elements of an `HList` according to the types specified by another
 * `HList` (or their respective subtypes). For each element of the selection list, exectly one
 * eligible element must exist in the list to select from
 *
 * @tparam L
 *   the `HList` type to select elements from
 * @tparam S
 *   the `HList` type to determine elements of which types need be selected; subtypes are
 *   eligible candidates
 */
sealed trait SelectEachUniqueCovariant[L <: HList, S <: HList] {

  def apply(list: L): S
}

object SelectEachUniqueCovariant {

  implicit def forNil[L <: HList]: SelectEachUniqueCovariant[L, HNil] =
    new SelectEachUniqueCovariant[L, HNil] {

      override def apply(list: L): HNil = HNil
    }

  implicit def forCons[L <: HList, H, T <: HList, R <: HList, R_](
      implicit sHead: SelectAllCovariant.Aux[L, H, R],
      sTail: SelectEachUniqueCovariant[L, T],
      isSingleton: R =:= (R_ :: HNil), // ensure the selected type is unique in the list to select from
      isOfRequestedType: R_ <:< H // require that it is indeed a subtype of the required type
  ): SelectEachUniqueCovariant[L, H :: T] =
    new SelectEachUniqueCovariant[L, H :: T] {

      override def apply(list: L): H :: T = sHead(list).head :: sTail(list)
    }
}
