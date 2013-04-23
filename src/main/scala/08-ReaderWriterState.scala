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

object ReaderWriterStateMonad {

  //YAY!
  //val config = Config( "  " )
  //val errors:mutable.ListBuffer[String] = mutable.ListBuffer()

  case class Config( indentSeq: String )
  type ElemStack = Stack[String]
  type PpReaderWriterState[A] = ReaderWriterState[Config,List[String],ElemStack, A]
  type PpProgram = PpReaderWriterState[List[String]]

  def stackHeight: PpReaderWriterState[Int] = ReaderWriterState{
    (r,s) => ( Nil , s.size, s )
  }

  def getIndentSeq: PpReaderWriterState[String] = ReaderWriterState{
    (r,s) => ( Nil , r.indentSeq, s )
  }

  def popStack: PpReaderWriterState[Unit] = ReaderWriterState{
    (r,s) => ( Nil , (), s.pop )
  }

  def pushStack( elem:String ): PpReaderWriterState[Unit] = ReaderWriterState{
    (r,s) => ( Nil , (), s.push(elem) )
  }

  def indented( text: String ):PpReaderWriterState[String] = for {
    level     <- stackHeight
    indentSeq <- getIndentSeq
  } yield (indentSeq * level) + text

  def verifyNewElement( event: XMLEvent ): PpReaderWriterState[Unit] =
    ReaderWriterState {
      (r,foundElems) => {
        val newLog = (foundElems.headOption,event) match {
          case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
            s"WARN: Msg should only contain text, contains: <$l>"
          )
          case _ => Nil
        }
        (newLog,(),foundElems)
      }
    }

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
      ReaderWriterState( (r,s) => (Nil,Nil,s) )
    )( (s,event) =>
      for {
        output    <- s
        newOutput <- indentEvent( event )
        _         <- verifyNewElement( event )
      } yield newOutput :: output
    )

    val (errors,lines,stack) = program.run( Config("  ") , Stack.empty )
    errors.foreach( System.err.println _ )
    lines.reverse.foreach( println _ )

  }

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }


}
