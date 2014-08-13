package shapeless.examples


import shapeless.PolyDefns._
import shapeless._
import shapeless.ops.hlist.At

import scala.math.Ordering

/**
 * Table and table sorting
 */
object table {

  class On[R<:HList] {

    type SO[A] = SelectNOrder[R, A]

    object transformer extends (SO ~>> String) {
      def apply[O](so: SO[O]): String =
        s"so.extractor=${so.extractor} with so.ord=${so.ord}"
    }

    case class State(pointer: Int, pageSize: Int, sortedRows: Seq[R])

  }

  trait Extractor[HF<:Nat, In <: HList, Remaining<: HList] extends DepFn0 { type Out <: HList }
  case class SelectNOrder[In <: HList, O](extractor: Function1[In,O], ord: Ordering[O])

  final case class myHListOps[L <: HList](l: L)  {

    def extractors(implicit extractor : Extractor[_0, L,L]) : extractor.Out = extractor()
  }

  object Extractor {
    def apply[HL <: HList]
    (implicit extractor: Extractor[_0, HL,HL]):
    Aux[_0, HL, HL, extractor.Out] = extractor

    type Aux[HF<:Nat, In <: HList, Remaining<: HList, Out0 <: HList] = Extractor[HF, In, Remaining] { type Out = Out0 }

    //To deal with case where HNil is passed. not sure if this is right.
    implicit def hnilExtractor: Aux[_0, HNil, HNil, HNil] =
      new Extractor[_0, HNil, HNil] {
        type Out = HNil
        def apply(): Out = HNil
      }

    implicit def hSingleExtractor1[N<:Nat, In<:HList, H ]
    (implicit att : At.Aux[In, N,H], ordering: Ordering[H]): Aux[N, In, H::HNil, SelectNOrder[In,H]::HNil] =
      new Extractor[N, In, H::HNil] {
        type Out = SelectNOrder[In,H]::HNil
        def apply(): Out = SelectNOrder[In,H](att.apply(_),ordering)::HNil
      }


    implicit def hlistExtractor1[N <: Nat, In<:HList, H, Tail<: HList]
    (implicit mt : Extractor[Succ[N], In, Tail],
     ordering: Ordering[H],
     att : At.Aux[In, N,H])
    :Aux[N, In, H::Tail, SelectNOrder[In,H]::mt.Out] = {
      new Extractor[N, In, H::Tail] {
        type Out = SelectNOrder[In,H]::mt.Out

        def apply(): Out = {
          SelectNOrder[In,H](att.apply(_),ordering):: mt()
        }
      }
    }
  }

  /**
   * Code taken from https://gist.github.com/milessabin/6814566
   */
  class Table[TH<:HList, TR<:HList](val hdrs: TH, val rows: Seq[TR])

  object Table {
    def apply[TH<:HList, TR<:HList](hdrs: TH, rows: Seq[TR])
                                   (implicit ts: TableShape[TH, TR]) = new Table(hdrs, rows)

    trait TableShape[TH, TR]

    object TableShape {
      implicit def productTableShape[TH, TR, LH, LR]
      (implicit
       //       genH: Generic.Aux[TH, LH],
       //       genR: Generic.Aux[TR, LR],
       hlistShape: TableShape[LH, LR]): TableShape[TH, TR] = new TableShape[TH, TR] {}

      implicit def hsingleTableShape[RH]: TableShape[String :: HNil, RH :: HNil] =
        new TableShape[String :: HNil, RH :: HNil] {}

      implicit def hlistTableShape[HT <: HList, RH, RT <: HList]
      (implicit tailShape: TableShape[HT, RT]): TableShape[String :: HT, RH :: RT] =
        new TableShape[String :: HT, RH :: RT] {}
    }

  }

  type LT = String::Boolean::HNil
  val simple = new On[LT]
  val l = "Hello"::true::HNil
//  val t = "num"::"String"::"Bool"::HNil

//  val table = Table(t,Seq(l))
  //val lo = myHListOps(l)
//  val x = lo.extractors
  val xx = Extractor.apply[LT]
  val x = xx()

  val result = x map simple.transformer
  println("result="+result)

}
