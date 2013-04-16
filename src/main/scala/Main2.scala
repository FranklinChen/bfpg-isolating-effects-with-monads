object Main2 {

  import scala.xml.pull._
  import scala.io.Source
  import scalaz._

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

  def printIndented( level: Int, text: String ):Reader[String,Unit] = Reader{
    (indentSeq) => println( (indentSeq * level) + text )
  }

  def main(filename: String) = {
    var indentLevel = 0
    def indenter( level: Int, text: String ) = ( "  " * level ) + text

    for ( event <- getStream( filename ) ) yield {
      event match {
        case EvComment(t) => printIndented( indentLevel , s"<!--$t-->" )
        case EvElemStart( _ , l , a , scope ) => {
          val out = printIndented( indentLevel , s"<$l>" )
          indentLevel = indentLevel + 1;
          out
        }
        case EvElemEnd( _ , l ) => {
          indentLevel = indentLevel - 1
          printIndented( indentLevel , s"</$l>" )
        }
        case EvText(t) => printIndented( indentLevel , t )
        case e => throw new RuntimeException( s"Can't match event: $e" )
      }
    }

  }

}
