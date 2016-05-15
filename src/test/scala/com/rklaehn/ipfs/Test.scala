package com.rklaehn.ipfs
import scala.concurrent._
import scala.util.{Failure, Success}

/**
  * Utility methods for console testing
  */
object Test {
  def p[T](f: Future[T])(implicit ec: ExecutionContext) = f.onComplete {
    case Success(x) => println(x)
    case Failure(e) => println(s"failure $e")
  }
}
