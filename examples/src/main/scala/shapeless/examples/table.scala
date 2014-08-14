package shapeless.examples


import java.util.Date

import shapeless.PolyDefns._
import shapeless._
import shapeless.ops.hlist.{Mapped, At, Length}
import shapeless.ops.nat.ToInt

import scala.math.Ordering

/**
 * Table and table sorting
 */
object table {

  class On[R<:HList] {

    type SO[A] = SelectNOrder[R, A]
    type Sorter = Seq[R] => Seq[R]

    object transformer extends (SO ~>> Sorter) {
      def apply[O](so: SO[O]): (Seq[R]=>Seq[R]) = (hlists: Seq[R]) => hlists.sortBy(so.extractor(_))(so.ord)
    }

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

  // This table view has methods to give one a view of the table
  // if the user clicks on a column header the view sorts the table
  // according to the ordering for that column of the hlist
  case class TableView[TH<:HList,TR<:HList, Out<:HList,Size<:Nat](table: Table[TH,TR], on: On[TR])
                                           (implicit extractor: Extractor.Aux[_0, TR,TR,Out],
                                            mapped: Mapped.Aux[TR, ({type l[a] = SelectNOrder[TR, a]})#l, Out],
                                            len: Length.Aux[TR, Size],
                                            toInt: ToInt[Size]) {
    val length: Int = toInt()



    var sortCol : Int = 0
    var sorters: List[Seq[TR]=>Seq[TR]] = {
//      def id(s: Seq[TR])=s
//      List(id,id,id)
      val xx = Extractor.apply[TR]
      val x = xx()
      val sorters = x map on.transformer
      sorters.toList
    }

    /*
    naive implementation of a click on header event.
    Safety is produced by modulus the length of the table
    In a fully developed UI the table view would sets the clickable
    fields itself, making it impossible to create out of bound click events
    */
    def clickOnHeader(col: Int){
      sortCol = col%length
    }

    def view = {
       val f = sorters(sortCol) // can throw index out of bounds exception!
       f(table.rows)
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
              genH: Generic.Aux[TH, LH],
              genR: Generic.Aux[TR, LR],
       hlistShape: TableShape[LH, LR]): TableShape[TH, TR] = new TableShape[TH, TR] {}

      implicit def hsingleTableShape[RH]: TableShape[String :: HNil, RH :: HNil] =
        new TableShape[String :: HNil, RH :: HNil] {}

      implicit def hlistTableShape[HT <: HList, RH, RT <: HList]
      (implicit tailShape: TableShape[HT, RT]): TableShape[String :: HT, RH :: RT] =
        new TableShape[String :: HT, RH :: RT] {}
    }

  }

  type LT = String::Boolean::Date::HNil
  val time = System.currentTimeMillis()
  val l = Seq("Hello"::false::new Date(time)::HNil,
              "Bye"::true::new Date(time+10000)::HNil,
              "Apple"::false::new Date(time-200000)::HNil)
  val t = "String"::"bool"::"Date"::HNil

  val table = Table(t,l)
  val tableView = TableView(table,new On)
  //val lo = myHListOps(l)
//  val x = lo.extractors

  println(tableView.view)
  tableView.clickOnHeader(1)
  println(tableView.view)
  tableView.clickOnHeader(2)
  println(tableView.view)

}
