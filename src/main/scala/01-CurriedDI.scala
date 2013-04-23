/*
 * In our quest to remove the global config, lets change indented to be curried
 * so that we can pre fill in the indentSeq and have that return a function
 * that'll indent a given string at a given level.
 *
 * This is equivalent to DI, just with functions instead of objects.
 *
 * This is pretty lame though, if three different functions that all need that
 * configuration then we're going to have to pass the config into each of the
 * functions to get back the configured functions. This would quickly become
 * cumbersome.
 */

import scala.xml.pull._
import scala.io.Source
import scala.collection.mutable

object CurriedDI {

  case class Config( indentSeq: String )
  //val indentSeq = Config( "  " )
  val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  var foundElems = mutable.Stack.empty[String]

  def indented( config: Config )( level: Int, text: String ) = {
    (config.indentSeq * level) + text
  }

  def verifyNewElement( event: XMLEvent ) = {
    (foundElems.headOption,event) match {
      case (Some("msg"),EvElemStart( _ , l , _ , _ )) => errors += (
        s"WARN: Msg should only contain text, contains: <$l>"
      )
      case _ => ()
    }
  }


  def main(filename: String) = {
    val indenter = indented( Config( "  " ) ) _
    val lines = for ( event <- getStream( filename ).toList ) yield {
      verifyNewElement( event )
      event match {
        case EvElemStart( _ , l , a , scope ) => {
          val out = indenter( foundElems.size , s"<$l>" )
          foundElems.push( l )
          out
        }
        case EvElemEnd( _ , l ) => {
          foundElems.pop()
          indenter( foundElems.size , s"</$l>" )
        }
        case EvText(t) => indenter( foundElems.size , t )
      }
    }

    errors.foreach( System.err.println _ )
    lines.foreach( println _ )

  }

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

}
