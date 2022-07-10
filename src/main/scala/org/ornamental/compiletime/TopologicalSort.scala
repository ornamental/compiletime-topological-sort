package org.ornamental.compiletime

import shapeless.HNil
import shapeless.HList

/**
 * Represents topologically sorted set of functions
 *
 * @tparam Funcs
 *   the heterogeneous list of function types to be sorted
 */
sealed trait TopologicalSort[Funcs <: HList] {

  /**
   * The heterogeneous list of return types of functions in `Funcs`, not necessarily in the
   * original order.
   */
  type Out <: HList

  /**
   * Provided a list of functions, sorts them and invokes them in the resulting order to provide
   * all the return values.
   *
   * @param funcs
   *   the functions to sort
   * @return
   *   the supplied functions' return values
   */
  def get(funcs: Funcs): Out
}

object TopologicalSort {

  private[TopologicalSort] class PartiallyApplied[Out <: HList](
      val dummy: Unit = ()
  ) extends AnyVal {

    def apply[Funcs_ <: HList, SortedOut <: HList](functions: Funcs_)(
        implicit ts: TopologicalSort.Aux[Funcs_, SortedOut],
        select: SelectEachUniqueCovariant[SortedOut, Out]): Out = select(ts.get(functions))
  }

  type Aux[Funcs_ <: HList, Out_ <: HList] = TopologicalSort[Funcs_] {
    type Out = Out_
  }

  implicit def fromStage[Funcs_ <: HList, Out_ <: HList](
      implicit topologicalSortStage: TopologicalSortStage.Aux[Funcs_, HNil, Out_])
      : TopologicalSort.Aux[Funcs_, Out_] =
    new TopologicalSort[Funcs_] {

      override type Out = Out_

      override def get(funcs: Funcs_): Out_ =
        topologicalSortStage.get(HNil, funcs)
    }

  /**
   * Sorts the given set of functions topologically.
   *
   * @tparam Out
   *   the return types of the supplied functions, in any order
   * @return
   *   the heterogeneous list of function output types, if topological sorting is possible
   */
  def sort[Out <: HList]: PartiallyApplied[Out] = new PartiallyApplied[Out]()
}
