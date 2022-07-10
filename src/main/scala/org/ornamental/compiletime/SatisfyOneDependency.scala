package org.ornamental.compiletime

import shapeless.::
import shapeless.HList
import shapeless.ops.function.FnToProduct

/**
 * Given a list of function types and a list of types, selects the first function whose input
 * types set is covered by the given types (so that each argument is uniquely satisfied by a
 * value from the given list).
 *
 * @tparam Funcs
 *   the type of heterogeneous list of functions to select one from
 * @tparam Givens
 *   the type of heterogeneous list of values to use as the selected function's arguments
 */
sealed trait SatisfyOneDependency[Funcs <: HList, Givens <: HList] {

  /**
   * The functions from the original list `Funcs` except for the selected one.
   */
  type RemainingFuncs <: HList

  /**
   * The return type of the selected function.
   */
  type SatisfiedFuncOut

  def produce(gvn: Givens, funcs: Funcs): SatisfiedFuncOut

  def remainingFuncs(funcs: Funcs): RemainingFuncs
}

sealed trait SatisfyOneDependencyLowPriority {

  type Aux[
      Funcs_ <: HList,
      Givens_ <: HList,
      RemainingFuncs_ <: HList,
      SatisfiedFuncOut_
  ] = SatisfyOneDependency[Funcs_, Givens_] {
    type RemainingFuncs = RemainingFuncs_
    type SatisfiedFuncOut = SatisfiedFuncOut_
  }

  implicit def findSatisfiableRecurse[
      FuncsHead,
      FuncsTail <: HList,
      Givens_ <: HList,
      RemainingFuncs_ <: HList,
      SatisfiedFuncOut_
  ](
      implicit base: Aux[
        FuncsTail,
        Givens_,
        RemainingFuncs_,
        SatisfiedFuncOut_
      ]): Aux[
    FuncsHead :: FuncsTail,
    Givens_,
    FuncsHead :: RemainingFuncs_,
    SatisfiedFuncOut_
  ] =
    new SatisfyOneDependency[FuncsHead :: FuncsTail, Givens_] {

      override type RemainingFuncs = FuncsHead :: RemainingFuncs_

      override type SatisfiedFuncOut = SatisfiedFuncOut_

      override def produce(
          gvn: Givens_,
          funcs: FuncsHead :: FuncsTail
      ): SatisfiedFuncOut_ = base.produce(gvn, funcs.tail)

      override def remainingFuncs(
          funcs: FuncsHead :: FuncsTail
      ): FuncsHead :: RemainingFuncs_ =
        funcs.head :: base.remainingFuncs(funcs.tail)
    }
}

object SatisfyOneDependency extends SatisfyOneDependencyLowPriority {

  implicit def findSatisfiableHead[
      FuncsHead,
      FuncsHeadIn <: HList,
      FuncsTail <: HList,
      Givens_ <: HList,
      SatisfiedFuncOut_
  ](
      implicit fnToProduct: FnToProduct.Aux[
        FuncsHead,
        FuncsHeadIn => SatisfiedFuncOut_
      ],
      selAll: SelectEachUniqueCovariant[Givens_, FuncsHeadIn]): SatisfyOneDependency.Aux[
    FuncsHead :: FuncsTail,
    Givens_,
    FuncsTail,
    SatisfiedFuncOut_
  ] =
    new SatisfyOneDependency[FuncsHead :: FuncsTail, Givens_] {

      override type RemainingFuncs = FuncsTail

      override type SatisfiedFuncOut = SatisfiedFuncOut_

      override def produce(
          gvn: Givens_,
          funcs: FuncsHead :: FuncsTail
      ): SatisfiedFuncOut_ = fnToProduct(funcs.head)(selAll(gvn))

      override def remainingFuncs(funcs: FuncsHead :: FuncsTail): FuncsTail =
        funcs.tail
    }
}
