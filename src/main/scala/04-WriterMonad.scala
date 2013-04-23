/*
 * Here we introduce the writer monad and change our verifyNewElement function
 * to return a Writer[List[String],Unit] monad instead. The writer monad, will
 * append our lists together when a writer is sequenced with another writer.
 *
 * List( "error" ).tell creates a writer of Writer[List[String],Unit]
 * if we wanted to return a value rather than unit, we can do:
 * 5.set( List("Error") ) creates a Writer[List[String],Int]
 *
 * Under the hood, scalaz is using the monoid typeclass under the hood to append
 * the logged state together. This means that writer is generic enough to be
 * able to append to any data structure imaginable so long as there is a monoid
 * instance for it (which is easy to do).
 *
 * We then bundle our writers inside of our previous readers, so the end result
 * of the first expression is List[Reader[String,Writer[List[String],String]]].
 *
 * The nesting there is getting annoying. We're only describing a computation
 * that is flat and sequential, so we shouldn't have to deal with these layers.
 */

import scala.xml.pull._
import scala.io.Source
import scala.collection.mutable
import scalaz._
import std.list._
import syntax.traverse._
import syntax.monad._
import syntax.writer._

object WriterMonad {

  case class Config( indentSeq: String )
  type PpWriter[+A] = Writer[List[String],A]
  type PpReader[+A] = Reader[Config,A]
  type PpProgram = PpReader[PpWriter[List[String]]]

  //val config = Config( "  " )
  //val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  var foundElems = mutable.Stack.empty[String]

  def getIndentSeq(): PpReader[String] = Reader { (conf) => conf.indentSeq }

  def indented( level: Int, text: String ):PpReader[String] =
    getIndentSeq().map( indentSeq => ( indentSeq * level) + text )

  def verifyNewElement( event: XMLEvent ): PpWriter[Unit] = {
    (foundElems.headOption,event) match {
      case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
        s"WARN: Msg should only contain text, contains: <$l>"
      ).tell
      case _ => Nil.tell
    }
  }

  def indentEvent( event: XMLEvent ): PpReader[String] = event match {
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
    val reader = getStream( filename ).toList.foldLeft[PpProgram](
      ( Nil.point[PpWriter] ).point[PpReader]
    )( (reader,event) => reader.flatMap( writer => {
      val newReader = indentEvent( event )
      val newWriter = verifyNewElement( event )

      newReader.map( newOutput =>
        writer.flatMap( outputs =>
          newWriter.map( _ => newOutput :: outputs )
        )
      )
    }))

    val (errors,lines) = reader( Config( "  " ) ).run
    errors.foreach( System.err.println _ )
    lines.reverse.foreach( println _ )

  }

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

}
