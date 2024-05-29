import undo._
import scala.concurrent.*
import scala.compiletime.summonInline
import scala.concurrent.duration.*

object undoEventually:

  final class UndoPatience(val duration: Duration)

  given UndoPatience = UndoPatience(1.minute)

  extension [T](fut: Future[T])
    /**
     * Adapts the compensator to wait for the future completion
     */
    infix inline def compensateEventually(compensator: Compensator[T]): Future[T] =
      val uc = summonInline[UndoContext]
      val up = summonInline[UndoPatience] 
      fut.compensate:
        case f: Future[T @unchecked] => 
          compensator(try{
            //this would be f.value using direct style futures
            Await.result(f, up.duration)
          }catch {
            case e: Exception => e
          })
        case e:Exception => compensator(e)
