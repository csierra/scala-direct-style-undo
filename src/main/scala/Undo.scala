import scala.util.boundary, boundary.break, boundary.Label
import scala.util.boundary.Break
import java.util.concurrent.ConcurrentLinkedDeque
import scala.jdk.CollectionConverters.*
import scala.compiletime.{summonInline, summonFrom}
import quoted.Type

type Compensator[T] = PartialFunction[T | Exception, Unit] 

class UndoContext:
  val compensators: ConcurrentLinkedDeque[() => Unit] = new ConcurrentLinkedDeque()
  def rollback: Unit = {
    compensators.asScala.foreach: compensator =>
      try{
        compensator()
      }
      catch {
        case e:Exception =>
      }
    }

object undo:
  /**
   * Defines a transaction block with a condition for the transaction to be rolledBack, based on the computed result of the block.
   * This assumes every block can deliver a result T or throw an Exception. If the result matches the predicate the compensators are run.
   */
  inline def rollbackOn[T](inline predicate: PartialFunction[T|Exception, Unit])(inline body: UndoContext ?=> T): T =
    val undoContext:UndoContext = new UndoContext
    val r : T | Exception = try {
      body(using undoContext)
    }
    catch {
      case e: Exception => e
    }

    if predicate.isDefinedAt(r) then
      undoContext.rollback
    r match {
      case e:Exception => throw e
      case t: T => t
    }

  inline def rollbackOnError[T](inline body: UndoContext ?=> T): T =
    undo.rollbackOn[T]({
      case e: Exception => 
    }):
      body

  extension [T](inline body: UndoContext ?=>T)
    infix inline def compensate(compensator: Compensator[T]): T =
      val uc: UndoContext = summonInline[UndoContext]
      try {
        val t : T = body(using uc)
        if compensator.isDefinedAt(t) then
          uc.compensators.push(() => compensator(t))
        t
      } catch {
        case e: Exception => 
          if compensator.isDefinedAt(e) then
            uc.compensators.push(() => compensator(e))
          throw e
      }

