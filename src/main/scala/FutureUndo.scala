import undo._
import scala.concurrent.*
import scala.compiletime.summonInline
import scala.concurrent.duration.*
import scala.util.{Success, Failure}

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

  /*
   * It should be possible to generalize this for a MonadError[Throwable], maybe with a rollbackOnM
   */
  inline def eventuallyRollbackOn[T](inline predicate: PartialFunction[T|Exception, Unit])(inline body: UndoContext ?=> Future[T])(using ExecutionContext): Future[T] =
    Future.successful(new UndoContext).flatMap(undoContext => body(using undoContext).transformWith {
      case Success(t) => 
        if predicate.isDefinedAt(t) then
          undoContext.rollback
        Future.successful(t)
      case Failure(e: Exception) => 
        if predicate.isDefinedAt(e) then
          undoContext.rollback
        Future.failed(e)
      case Failure(t) => Future.failed(t)
    })
