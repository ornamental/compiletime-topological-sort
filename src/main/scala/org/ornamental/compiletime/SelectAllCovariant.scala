package org.ornamental.compiletime

import shapeless.{::, HList, HNil}

/**
 * Type family selecting elements of an `HList` by type or subtype.
 *
 * @tparam L
 *   the `HList` type to select elements from
 * @tparam T
 *   the type to select elements by; subtypes are eligible candidates
 */
sealed trait SelectAllCovariant[L <: HList, T] {

  /**
   * The resulting selection type (subsequence of elements of `L`).
   */
  type Selected <: HList

  def apply(list: L): Selected
}

sealed trait LowPrioritySelectAllCovariant {

  type Aux[L <: HList, T, S <: HList] = SelectAllCovariant[L, T] {
    type Selected = S
  }

  implicit def matchTail[A, H, T <: HList, R <: HList](
      implicit rest: SelectAllCovariant.Aux[T, A, R]): SelectAllCovariant.Aux[H :: T, A, R] =
    new SelectAllCovariant[H :: T, A] {

      override type Selected = R

      override def apply(list: H :: T): R = rest(list.tail)
    }
}

object SelectAllCovariant extends LowPrioritySelectAllCovariant {

  implicit def base[A]: SelectAllCovariant.Aux[HNil, A, HNil] =
    new SelectAllCovariant[HNil, A] {

      override type Selected = HNil

      override def apply(list: HNil): HNil = HNil
    }

  implicit def matchHead[A, H <: A, T <: HList, R <: HList](
      implicit
      rest: SelectAllCovariant.Aux[T, A, R]): SelectAllCovariant.Aux[H :: T, A, H :: R] =
    new SelectAllCovariant[H :: T, A] {

      override type Selected = H :: R

      override def apply(list: H :: T): H :: R = list.head :: rest(list.tail)
    }
}
