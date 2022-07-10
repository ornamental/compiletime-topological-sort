package org.ornamental.compiletime

import shapeless.::
import shapeless.HList
import shapeless.HNil

/**
 * Intermediate result of topological sort.
 *
 * @tparam SortedFuncs
 *   the heterogeneous list of the function types to sort; becomes longer with every stage
 * @tparam GivenVals
 *   the heterogeneous list of given values which may be used as function parameters; shortens
 *   with each stage
 */
sealed trait TopologicalSortStage[
    SortedFuncs <: HList,
    GivenVals <: HList
] {

  /**
   * The heterogeneous list of values produced at this stage (proper value added at this stage
   * plus all the values produced by the previous stages).<br/> The element types are the same
   * as return types of elements of `SortedFuncs`, but not necessarily in the same order.
   */
  type Out <: HList

  def get(givenVals: GivenVals, funcs: SortedFuncs): Out
}

object TopologicalSortStage {

  type Aux[Funcs_ <: HList, Givens_ <: HList, Out_ <: HList] =
    TopologicalSortStage[Funcs_, Givens_] {
      type Out = Out_
    }

  implicit def recurse[
      Funcs_ <: HList,
      Givens_ <: HList,
      RemainingFuncs_ <: HList,
      SatisfiedFuncOut_,
      Out_ <: HList
  ](
      implicit satisfy1: SatisfyOneDependency.Aux[
        Funcs_,
        Givens_,
        RemainingFuncs_,
        SatisfiedFuncOut_
      ],
      baseStage: TopologicalSortStage.Aux[
        RemainingFuncs_,
        SatisfiedFuncOut_ :: Givens_,
        Out_
      ]): TopologicalSortStage.Aux[Funcs_, Givens_, SatisfiedFuncOut_ :: Out_] =
    new TopologicalSortStage[Funcs_, Givens_] {

      override type Out = SatisfiedFuncOut_ :: Out_

      override def get(
          givenVals: Givens_,
          funcs: Funcs_
      ): SatisfiedFuncOut_ :: Out_ = {
        val newOut: SatisfiedFuncOut_ = satisfy1.produce(givenVals, funcs)
        newOut :: baseStage.get(
          newOut :: givenVals,
          satisfy1.remainingFuncs(funcs)
        )
      }
    }

  implicit def forNil[Givens_ <: HList]: TopologicalSortStage.Aux[HNil, Givens_, HNil] =
    new TopologicalSortStage[HNil, Givens_] {

      override type Out = HNil

      override def get(givenVals: Givens_, funcs: HNil): HNil = HNil
    }
}
