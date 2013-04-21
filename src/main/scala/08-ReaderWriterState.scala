/*
 *
 */
object ReaderWriterStateMonad {

  import scala.xml.pull._
  import scala.io.Source
  import scala.collection.immutable.Stack
  import scalaz._
  import std.list._
  import syntax.traverse._
  import syntax.comonad._
  import syntax.monad._
  import syntax.writer._

  type ElemStack = Stack[String]
  type PpReaderWriterState[A] = ReaderWriterState[String,List[String],ElemStack,A]
  type PpProgram = PpReaderWriterState[List[String]]

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

  def stackHeight: PpReaderWriterState[Int] = ReaderWriterState{
    (r,s) => ( Nil , s.size, s )
  }

  def getIndentSeq: PpReaderWriterState[String] = ReaderWriterState{
    (r,s) => ( Nil , r, s )
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
            s"WARN: <$l> shouldn't be within <msg>. Msg should only contain text."
          )
          case _ => Nil
        }
        (newLog,(),foundElems)
      }
    }

  //val indentSeq = "  "
  //val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  //var foundElems = mutable.Stack.empty[String]

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
      s.flatMap( lines =>
        indentEvent( event ).flatMap( line =>
          verifyNewElement( event ).map( _ =>
            line::lines
          )
        )
      )
    )

    val (errors,lines,stack) = program.run( "  " , Stack.empty )
    errors.foreach( System.err.println _ )
    lines.reverse.foreach( println _ )

  }

}
