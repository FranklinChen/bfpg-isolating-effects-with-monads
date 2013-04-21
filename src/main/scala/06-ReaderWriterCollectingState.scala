/*
 * Removing the mutable stack is something that is different and a bit trickier
 * to deal with compared to the read-only / write-only issues that we cleaned up
 * before. This is because each step of the computation depends on the state of
 * the previous step so we have to thread it through and make it readable and
 * writable.
 *
 * To do this, we're going to use a fold to recurse the stream and keep track
 * of the state and results that we're collecting.
 *
 * This is really crappy though. We're having to juggle the state each iteration
 * regardless of whether we're changing the stack or not
 */

object ReaderWriterCollectingState {

  import scala.xml.pull._
  import scala.io.Source
  import scala.collection.immutable.Stack
  import scalaz._
  import std.list._
  import syntax.traverse._
  import syntax.comonad._
  import syntax.monad._
  import syntax.writer._

  type PpWriter[+A] = Writer[List[String],A]
  type PpReaderWriter[+A] = ReaderT[PpWriter,String,A]
  type PpReaderWriterWithState = PpReaderWriter[(Stack[String],List[String])]

  def getStream(filename: String) = {
    new XMLEventReader(Source.fromFile(filename)).toStream
  }

  def indented( level: Int, text: String ):PpReaderWriter[String] = Kleisli{
    (indentSeq:String) => ((indentSeq * level) + text).point[PpWriter]
  }

  def verifyNewElement(
    foundElems: Stack[String]
    ,event: XMLEvent
  ): PpReaderWriter[Unit] = Kleisli{ _ =>
     (foundElems.headOption,event) match {
       case (Some("msg"),EvElemStart( _ , l , _ , _ )) => List(
         s"WARN: <$l> shouldn't be within <msg>. Msg should only contain text."
       ).tell
       case _ => Nil.tell
     }
  }

  //val indentSeq = "  "
  //val errors:mutable.ListBuffer[String] = mutable.ListBuffer()
  //var foundElems = mutable.Stack.empty[String]

  def indentEvent(
    foundElems: Stack[String]
    , event: XMLEvent
  ):PpReaderWriter[(Stack[String],String)] = event match {
    case EvComment(t) =>
      indented( foundElems.size , s"<!--$t-->" ).map( ( foundElems, _ ) )
    case EvElemStart( _ , l , _ , _ ) => {
      val out = indented( foundElems.size , s"<$l>" )
      out.map( ( foundElems.push( l ) , _ ) )
    }
    case EvElemEnd( _ , l ) => {
      val newStack = foundElems.pop
      indented( newStack.size , s"</$l>" ).map( ( newStack , _ ) )
    }
    case EvText(t) => indented( foundElems.size , t ).map( ( foundElems , _ ) )
    case e => throw new RuntimeException( s"Can't match event: $e" )
  }

  def main(filename: String) = {
    val readerWriter = getStream( filename ).foldLeft[PpReaderWriterWithState](
      Kleisli{ _:String => (Stack.empty , Nil).point[PpWriter] }
    )( (rw,event) => {
      for{
        s1 <- rw
        s2 <- indentEvent( s1._1 , event )
        _  <- verifyNewElement( s1._1 , event )
      } yield (s2._1,s2._2::s1._2)
    } )

    val (errors,(_,lines)) = readerWriter.run( "  " ).run
    errors.foreach( System.err.println _ )
    lines.reverse.foreach( println _ )

  }

}
