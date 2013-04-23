/*
 * We can get rid of the mutable error global by making each event map to a
 * tuple that holds list of errors and the resulting pretty printed line. That
 * way we can generate all of our error lines and append them all together at
 * the end.
 *
 * This only really works because we were already collecting a list. If our
 * log lines were a different shape to our output (say we're logging a list of
 * lines but only returning a single result) we'd be forcing the caller of our
 * functions to join the resulting lists for us at each level, which is hideous.
 *
 * This appending ( |+| ) is done with the magic of the monoid typeclass which
 * includes a zero element (in the list case, this is nil) and an append
 * operation (in the case of a list, this is ++). Using the monoid append here
 * is useless because we already know that we're dealing with a list, but seeing
 * it here demonstrates how scalaz manages the next step.
 *
 * The writer monad was written to solve this append only state problem. We'll
 * see this in the next step!
 */

import scala.xml.pull._
import scala.io.Source
import scala.collection.mutable
import scalaz._
import std.list._
import syntax.traverse._
import syntax.monad._

object CollectingErrorList {

  case class Config( indentSeq: String )
  //val config = Config( "  " )
  val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  var foundElems = mutable.Stack.empty[String]

  type PpReader[+A] = Reader[Config,A]
  type PpProgram = PpReader[(List[String],List[String])]

  def getIndentSeq(): PpReader[String] = Reader { (conf) => conf.indentSeq }

  def indented( level: Int, text: String ):PpReader[String] =
    getIndentSeq().map( indentSeq => ( indentSeq * level) + text )

  def verifyNewElement( event: XMLEvent ) = {
    (foundElems.headOption,event) match {
      case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
        s"WARN: Msg should only contain text, contains: <$l>"
      )
      case _ => Nil
    }
  }

  def main(filename: String) = {
    val result = getStream( filename ).toList.foldLeft[PpProgram](
      (Nil,Nil).point[PpReader]
    )( (reader,event) => reader.flatMap{ case (errors,outputs) => {
      val newErrors = verifyNewElement(event)
      val newReader = event match {
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
      newReader.map(newOutput => (errors ++ newErrors , newOutput :: outputs))
    }})

    val (errors,output) = result( Config( "  " ) )
    errors.foreach( System.err.println _ )
    output.reverse.foreach( println _ )

  }

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }


}
