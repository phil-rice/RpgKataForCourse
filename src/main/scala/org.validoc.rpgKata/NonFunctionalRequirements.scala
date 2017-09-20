package org.validoc.rpgKata

import java.text.MessageFormat
import java.util.concurrent.atomic.AtomicInteger


trait LogData[T] extends (T => String)

object LogData {
  implicit def defaultLogData[T] = new LogData[T] {
    override def apply(v1: T) = v1.toString
  }

//  implicit object CharacterLogData extends LogData[Character] {
//    override def apply(v1: Character) = v1.name + " is alive: " + v1.alive + " with " + v1.hitPoints.hp
//  }

}

/** example pattern would be 'damage {0} => {1}' */
class Logging[From, To](pattern: String, delegate: From => To)(implicit logDataF: LogData[From], logDataT: LogData[To]) extends (From => To) {
  override def apply(from: From) = {
    val result = delegate(from)
    println(MessageFormat.format(pattern, logDataF(from), logDataT(result)))
    result
  }
}

class Metrics[From, To](store: AtomicInteger, delegate: From => To) extends (From => To) {
  override def apply(from: From) = {
    val result = delegate(from)
    store.incrementAndGet()
    result
  }
}

class ErrorHandler[From, To](delegate: From => To) extends (From => To) {
  override def apply(from: From) = try {
    delegate(from)
  } catch {
    case e: Exception => println("Doing some logging here" + e); throw e
  }
}

class NonFunctionalRequirements[From,To] {
  def logging(pattern: String) = { (delegate: From => To) => new Logging[From, To](pattern, delegate) }
  def metrics(atomicInteger: AtomicInteger) = { (delegate: From => To) => new Metrics(atomicInteger, delegate) }
  def error = { (delegate: From => To) => new ErrorHandler(delegate) }
}

