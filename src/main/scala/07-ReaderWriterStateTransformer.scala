/*
 *
 */

object ReaderWriterStateTransformer {

  import scala.xml.pull._
  import scala.io.Source
  import scala.collection.immutable.Stack
  import scalaz._
  import std.list._
  import syntax.traverse._
  import syntax.comonad._
  import syntax.monad._
  import syntax.writer._

  type ElemStack = Stack[String]
  type PpWriter[+A] = Writer[List[String],A]
  type PpState[+A] = StateT[PpWriter,ElemStack,A]
  type PpReaderWriterState[+A] = ReaderT[PpState,String,A]
  type PpProgram = PpReaderWriterState[List[String]]

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

  def stackHeight: PpReaderWriterState[Int] =
    Kleisli[PpState,String,Int]{ _ =>
      StateT[PpWriter,ElemStack,Int]{ s:ElemStack => (s,s.size).set(Nil) }
    }

  def popStack: PpReaderWriterState[Unit] =
    Kleisli[PpState,String,Unit]{ _ =>
      StateT[PpWriter,ElemStack,Unit]{ s:ElemStack => (s.pop,()).set(Nil) }
    }

  def pushStack( newElem:String ): PpReaderWriterState[Unit] =
    Kleisli[PpState,String,Unit]{ _ =>
      StateT[PpWriter,ElemStack,Unit]{
        s:ElemStack => (s.push(newElem),()).set(Nil)
      }
    }

  def getIndentSeq: PpReaderWriterState[String] = Kleisli.ask[PpState,String]

  def indented( text: String ):PpReaderWriterState[String] = for {
    level     <- stackHeight
    indentSeq <- getIndentSeq
  } yield (indentSeq * level) + text

  def verifyNewElement( event: XMLEvent ): PpReaderWriterState[Unit] =
    Kleisli[PpState,String,Unit]{ _ =>
      StateT[PpWriter,ElemStack,Unit]{ foundElems =>
        (foundElems,()).set(
          (foundElems.headOption,event) match {
            case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
              s"WARN: <$l> shouldn't be within <msg>. Msg should only contain text."
            )
            case _ => Nil
          }
        )
      }
    }

  //val indentSeq = "  "
  //val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  //var foundElems = mutable.Stack.empty[String]

  def indentEvent( event: XMLEvent ):PpReaderWriterState[String] =
    event match {
      case EvComment(t) => indented( s"<!--$t-->" )
      case EvElemStart( _ , l , _ , _ ) => for{
        line <- indented( s"<$l>" )
        _    <- pushStack( l )
      } yield line
      case EvElemEnd( _ , l ) => for {
        _    <- popStack
        line <- indented( s"</$l>" )
      } yield line
      case EvText(t) => indented( t )
      case e => throw new RuntimeException( s"Can't match event: $e" )
    }

  def main(filename: String) = {
    val program = getStream( filename ).foldLeft[PpProgram](
      Kleisli{ s:String => Nil.point[PpState] }
    )( (s,event) =>
      s.flatMap( lines =>
        indentEvent( event ).flatMap( line =>
          verifyNewElement( event ).map( _ =>
            line::lines
          )
        )
      )
    )

    val (errors,(_,lines)) = program.run( "  " ).run( Stack.empty ).run
    errors.foreach( System.err.println _ )
    lines.reverse.foreach( println _ )

  }

}
