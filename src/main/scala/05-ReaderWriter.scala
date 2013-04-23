/*
 *
 */

import scala.xml.pull._
import scala.io.Source
import scala.collection.mutable
import scalaz._
import std.list._
import syntax.traverse._
import syntax.comonad._
import syntax.monad._
import syntax.writer._

object ReaderWriterMonad {

  case class Config( indentSeq: String )
  type PpWriter[+A] = Writer[List[String],A]
  type PpReaderWriter[+A] = ReaderT[PpWriter,Config,A]
  type PpProgram = PpReaderWriter[List[String]]

  //val config = Config( "  " )
  //val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  var foundElems = mutable.Stack.empty[String]

  def indented( level: Int, text: String ):PpReaderWriter[String] = Kleisli{
    (conf) => ((conf.indentSeq * level) + text).point[PpWriter]
  }

  def verifyNewElement( event: XMLEvent ): PpReaderWriter[Unit] = Kleisli{ _ =>
     (foundElems.headOption,event) match {
       case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
         s"WARN: Msg should only contain text, contains: <$l>"
       ).tell
       case _ => Nil.tell
     }
  }

  def indentEvent( event: XMLEvent ):PpReaderWriter[String] = event match {
    case EvElemStart( _ , l , _ , _ ) => {
      val out = indented( foundElems.size , s"<$l>" )
      foundElems.push( l )
      out
    }
    case EvElemEnd( _ , l ) => {
      foundElems.pop()
      indented( foundElems.size , s"</$l>" )
    }
    case EvText(t) => indented( foundElems.size , t )
  }

  def main(filename: String) = {
    val readerWriter = getStream( filename ).toList.foldLeft[PpProgram](
      Nil.point[PpReaderWriter]
    )( (readerWriter,event) => for{
        output    <- readerWriter
        newOutput <- indentEvent( event )
        _         <- verifyNewElement( event )
      } yield newOutput :: output
    )

    val (errors,lines) = readerWriter( Config( "  " ) ).run
    errors.foreach( System.err.println _ )
    lines.reverse.foreach( println _ )

  }

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

}
