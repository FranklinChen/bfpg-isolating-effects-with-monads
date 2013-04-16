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

object Main3 {

  import scala.xml.pull._
  import scala.io.Source
  import scala.collection.mutable
  import scalaz._
  import std.list._
  import syntax.traverse._
  import syntax.monoid._

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

  var foundElems = mutable.Stack.empty[String]

  def indented( level: Int, text: String ):Reader[String,String] = Reader{
    (indentSeq) => (indentSeq * level) + text
  }

  def verifyNewElement( event: XMLEvent ) = {
    (foundElems.headOption,event) match {
      case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
        s"WARN: <$l> shouldn't be within <msg>. Msg should only contain text."
      )
      case _ => Nil
    }
  }

  def main(filename: String) = {
    val result = for ( event <- getStream( filename ).toList ) yield {
      val errors = verifyNewElement(event)
      val prettyPrintedLine = event match {
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

      (errors,prettyPrintedLine)
    }

    //This is equivalent to this (because we're using the List monoid):
    // result.map( _._1 ).foldRight[List[String]]( Nil )( _ ++ _ )
    result.map( _._1 ).foldRight[List[String]]( mzero )( _ |+| _ ).foreach(
      System.err.println _
    )

    result.map( _._2 ).sequenceU.apply( " " ).foreach( println _ )

  }

}
