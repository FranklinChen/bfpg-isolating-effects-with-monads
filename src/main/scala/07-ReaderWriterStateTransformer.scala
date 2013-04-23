/*
 *
 */

import scala.xml.pull._
import scala.io.Source
import scala.collection.immutable.Stack
import scalaz._
import std.list._
import syntax.traverse._
import syntax.comonad._
import syntax.monad._
import syntax.writer._

object ReaderWriterStateTransformer {

  //YAY!
  //val config = Config( "  " )
  //val errors:mutable.ListBuffer[String] = mutable.ListBuffer()

  case class Config( indentSeq: String )
  type ElemStack = Stack[String]
  type PpWriter[+A] = Writer[List[String],A]
  type PpState[+A] = StateT[PpWriter,ElemStack,A]
  type PpReaderWriterState[+A] = ReaderT[PpState,Config,A]
  type PpProgram = PpReaderWriterState[List[String]]

  def stackHeight: PpReaderWriterState[Int] =
    Kleisli[PpState,Config,Int]{ _ =>
      StateT[PpWriter,ElemStack,Int]{ s => (s,s.size).point[PpWriter] }
    }

  def popStack: PpReaderWriterState[Unit] =
    Kleisli[PpState,Config,Unit]{ _ =>
      StateT[PpWriter,ElemStack,Unit]{ s => (s.pop,()).point[PpWriter] }
    }

  def pushStack( newElem:String ): PpReaderWriterState[Unit] =
    Kleisli[PpState,Config,Unit]{ _ =>
      StateT[PpWriter,ElemStack,Unit]{
        s => (s.push(newElem),()).point[PpWriter]
      }
    }

  def getIndentSeq: PpReaderWriterState[String] =
    Kleisli.ask[PpState,Config].map( _.indentSeq )

  def indented( text: String ):PpReaderWriterState[String] = for {
    level     <- stackHeight
    indentSeq <- getIndentSeq
  } yield (indentSeq * level) + text

  def verifyNewElement( event: XMLEvent ): PpReaderWriterState[Unit] =
    Kleisli[PpState,Config,Unit]{ _ =>
      StateT[PpWriter,ElemStack,Unit]{ foundElems =>
        (foundElems,()).set(
          (foundElems.headOption,event) match {
            case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
              s"WARN: Msg should only contain text, contains: <$l>"
            )
            case _ => Nil
          }
        )
      }
    }

  def indentEvent( event: XMLEvent ):PpReaderWriterState[String] =
    event match {
      case EvElemStart( _ , l , _ , _ ) => for{
        line <- indented( s"<$l>" )
        _    <- pushStack( l )
      } yield line
      case EvElemEnd( _ , l ) => for {
        _    <- popStack
        line <- indented( s"</$l>" )
      } yield line
      case EvText(t) => indented( t )
    }

  def main(filename: String) = {
    val program = getStream( filename ).foldLeft[PpProgram](
      Kleisli{ _ => Nil.point[PpState] }
    )( (s,event) =>
      for {
        output    <- s
        newOutput <- indentEvent( event )
        _         <- verifyNewElement( event )
      } yield newOutput :: output
    )

    val (errors,(_,lines)) = program.run( Config("  ") ).run( Stack.empty ).run
    errors.foreach( System.err.println _ )
    lines.reverse.foreach( println _ )

  }

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

}
