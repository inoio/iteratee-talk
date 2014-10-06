package controllers

import play.api.Play.current
import play.api.http.Writeable
import play.api.libs.EventSource
import play.api.libs.EventSource.EventDataExtractor
import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.ws._
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Application extends Controller {

  val X_HELP: String = "X-Help"

  case class Element(number: Int = scala.util.Random.nextInt())

  implicit val elementFormat: Format[Element] = Json.format[Element]

  implicit val writeableElement: Writeable[Element] = new Writeable[Element](element => Json.toJson(element).toString.getBytes, Some(JSON))

  implicit val writeableSeqElement: Writeable[Seq[Element]] = new Writeable[Seq[Element]](elements => Json.toJson(elements).toString.getBytes(), Some(JSON))


  def safeParse(string: String): Try[Element] = {
    for {
                                        jsValue <- Try {
                                          Json.parse(string)
                                        }
                                        element <- jsValue.asOpt[Element] match {
                                          case Some(element) => Success(element)
                                          case None => Failure(new IllegalArgumentException(string))
                                        }
    } yield element
  }

  // step1 : produce an endless enumerator of Json objects
  val _step1: Enumerator[Element] = ???

  // step2 : produce an enumerator with 10 elements from elements
  val _step2 = ???

  // step3 : produce an enumerator only with positive numbers and up to 10
  val _step3 = ???

  // Convert an Array[Byte] enumerator to an [Byte] enumerator
  val toBytes: Enumeratee[Array[Byte], Byte] = ???

  // find a particular character in the "stream"
  def findChar(character: Byte): Iteratee[Byte, Unit] = ???


  // consume the stream until a particular character is found
  def consumeUntil(character: Byte, found: String = ""): Iteratee[Byte, String] = ???

  // alternative version of findChar
  def findChar2(character: Byte) = ???

  val findLeftBrace = findChar('{'.toByte)

  val consumeUntilRightBrace = consumeUntil('}')

  // process 1 Element from the byte stream
  val processElement: Iteratee[Byte, Element] = ???

  // produce a sequence of elements from the byte stream
  val processElements: Iteratee[Byte, Seq[Element]] = ???

  implicit class JsonIteratee[E, A](inner: Iteratee[E, A]) {
    def asJson(implicit F: Writes[A]): Iteratee[E, JsValue] = inner map (e => Json.toJson(e))
  }

  // step4: consume 10 positives and build the sum
  val sum: Iteratee[Element, Int] = ???

  // step4: Alternative to building the sum
  def sum2(): Iteratee[Element, Int] = ???

  def step1 = TODO

  def step2 = TODO

  def step3 = TODO

  def step4 = TODO

  def step5 = TODO

  def step6 = TODO

  def step7 = TODO

  def step8 = TODO

}