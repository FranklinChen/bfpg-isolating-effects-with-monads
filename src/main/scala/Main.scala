/*
 * Our first imperative attempt at writing a solution. Our problem is as such:
 * - We need to read in an XML file and pretty print it with a configurable
 *   indentation sequence (two spaces, a tab, etc.).
 * - We also need to 'validate' the XML and print out warnings if we find bogus
 *   things (but still pretty print the result anyway). To keep things simple,
 *   the only rule we have is that the <msg> element can't contain sub elements.
 *
 * This first attempt has a subtle bug around the laziness of the stream. Our
 * errors never actually get printed because the errors variable isn't appended
 * to till the stream is traversed in the last foreach.
 *
 * While the laziness isn't particularly useful to us here, there are a lot of
 * FP solutions that rely on laziness to be efficient. We're going to have to
 * get rid of the variables if we're to be able to use non-strict solutions.
 *
 * This is where monads come into play. A monad describes a computational
 * context that can be sequenced with other contexts of that same type. If we
 * can model our side effects in a monad, we're being explicit that these side
 * effects can only be done in a sequential fashion.
 *
 * First, lets fix that global indentSeq 'config'. Hardcoded configs like this
 * make code brittle and difficult to test.
 */

object Main {

  import scala.xml.pull._
  import scala.io.Source
  import scala.collection.mutable

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

  val indentSeq = "  "
  val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  var foundElems = mutable.Stack.empty[String]

  def indented( indentLevel: Int , text: String ) = {
    ( indentSeq * indentLevel ) + text
  }

  def verifyNewElement( event: XMLEvent ) = {
    (foundElems.headOption,event) match {
      case (Some("msg"),EvElemStart( _ , l , _ , _ )) => errors += (
        s"WARN: <$l> shouldn't be within <msg>. Msg should only contain text."
      )
      case _ => ()
    }
  }

  def main(filename: String) = {
    val lines = for ( event <- getStream( filename ) ) yield {
      verifyNewElement( event )
      event match {
        case EvComment(t) => indented( foundElems.size , s"<!--$t-->" )
        case EvElemStart( _ , l , a , scope ) => {
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
    }

    errors.foreach( System.err.println _ )
    lines.foreach( println _ )

  }


}
