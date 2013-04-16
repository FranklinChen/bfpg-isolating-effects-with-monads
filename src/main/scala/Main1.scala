object Main1 {

  import scala.xml.pull._
  import scala.io.Source

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

  def printIndented( indentSeq: String )( level: Int, text: String ) = {
    println( (indentSeq * level) + text )
  }

  def main(filename: String) = {
    var indentLevel = 0
    val indenter = printIndented( "  " ) _

    for ( event <- getStream( filename ) ) {
      event match {
        case EvComment(t) => indenter( indentLevel , s"<!--$t-->" )
        case EvElemStart( _ , l , a , scope ) => {
          indenter( indentLevel , s"<$l>" )
          indentLevel = indentLevel + 1;
        }
        case EvElemEnd( _ , l ) => {
          indentLevel = indentLevel - 1
          indenter( indentLevel , s"</$l>" )
        }
        case EvText(t) => indenter( indentLevel , t )
        case e => throw new RuntimeException( s"Can't match event: $e" )
      }
    }

  }

}
