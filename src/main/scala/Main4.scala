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
 * We then bundle our readers inside of the writers, so the end result of the
 * first expression is List[Writer[List[String],Reader[String,String]]].
 *
 * The nesting there is getting annoying, but lets remove that mutable stack
 * first.
 */

object Main4 {

  import scala.xml.pull._
  import scala.io.Source
  import scala.collection.mutable
  import scalaz._
  import std.list._
  import syntax.traverse._
  import syntax.writer._

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

  var foundElems = mutable.Stack.empty[String]

  def indented( level: Int, text: String ):Reader[String,String] = Reader{
    (indentSeq) => (indentSeq * level) + text
  }

  def verifyNewElement( event: XMLEvent ): Writer[List[String],Unit] = {
    (foundElems.headOption,event) match {
      case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
        s"WARN: <$l> shouldn't be within <msg>. Msg should only contain text."
      ).tell
      case _ => Nil.tell
    }
  }

  def main(filename: String) = {
    val writers = for ( event <- getStream( filename ).toList ) yield {
      verifyNewElement(event).map( _ =>
        event match {
          case EvComment(t) => indented( foundElems.size , s"<!--$t-->" )
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
          case e => throw new RuntimeException( s"Can't match event: $e" )
        }
      )
    }

    val (errors,readers) = writers.sequenceU.run
    errors.foreach( System.err.println _ )
    readers.sequenceU.apply( " " ).foreach( println _ )

  }

}
