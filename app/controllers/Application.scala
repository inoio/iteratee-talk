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

  // step1 : produce an endless enumerator of Json objects
  val _step1: Enumerator[Element] = Enumerator.repeat(Element())

  // step2 : produce an enumerator with 10 elements from elements
  val _step2 = _step1 &> Enumeratee.take(10)

  // step3 : produce an enumerator only with positive numbers and up to 10
  val _step3 = _step1 &> Enumeratee.filter {
    _.number > 0
  } &> Enumeratee.take(1000)

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

  val toBytes: Enumeratee[Array[Byte], Byte] = Enumeratee.mapFlatten[Array[Byte]] { e => Enumerator.enumerate(e)}

  def findChar(character: Byte): Iteratee[Byte, Unit] = Cont {
    case Input.EOF => Done(())
    case Input.Empty => findChar(character)
    case Input.El(c) => if (c == character) Done((), Input.El(character)) else findChar(character)
  }

  def consumeUntil(character: Byte, found: String = ""): Iteratee[Byte, String] = Cont {
    case Input.EOF => Done(found)
    case Input.Empty => consumeUntil(character, found)
    case Input.El(char) => if (char != character) consumeUntil(character, found + char.toChar) else Done(found + char.toChar)
  }

  def findChar2(character: Byte) = Enumeratee.takeWhile[Byte](_ != character)

  val findLeftBrace = findChar('{'.toByte)

  val consumeUntilRightBrace = consumeUntil('}')

  val processElement: Iteratee[Byte, Element] = for {
    _ <- findLeftBrace
    string <- consumeUntilRightBrace
    element <- {
      safeParse(string) match {
        case Success(element) => Done[Byte, Element](element)
        case Failure(_) => Error[Byte](s"$string not a valid element", Input.EOF)
      }
    }
  } yield element

  val processElements: Iteratee[Byte, Seq[Element]] = Iteratee.repeat[Byte, Element] {
    processElement
  }

  implicit class JsonIteratee[E, A](inner: Iteratee[E, A]) {
    def asJson(implicit F: Writes[A]): Iteratee[E, JsValue] = inner map (e => Json.toJson(e))
  }

  // step4: consume 10 positives and build the sum
  val sum: Iteratee[Element, Int] = {
    def summit(currentSum: Int): Iteratee[Element, Int] = Cont {
      case Input.Empty => summit(currentSum)
      case Input.EOF => Done(currentSum, Input.EOF)
      case in@Input.El(e) =>
        val number = Json.toJson(e).asOpt[Element]
        number map (element => summit(currentSum + element.number)) getOrElse (Error("Meeh", in))
    }

    summit(0)
  }

  // step4: Alternative to building the sum
  def sum2(): Iteratee[Element, Int] = Iteratee.fold(0) { case (sum, element) => sum + element.number}

  def step1 = Action {
    Ok.chunked(_step1)
  }

  def step2 = Action {
    Ok.chunked(_step2).withHeaders(X_HELP -> "Just consume 10 elements")
  }

  def step3 = Action {
    Ok.chunked(_step3).withHeaders(X_HELP -> "Filter elements so that only positive ones are available")
  }

  def step4 = Action.async {
    for {
      value <- _step3 |>>> sum2()
    } yield Ok(Element(value)).withHeaders(X_HELP -> "Compute the sum from elements in an enumerator")
  }

  def step5 = Action.async {
    val stream: Future[(WSResponseHeaders, Enumerator[Array[Byte]])] = WS.url("http://localhost:9000/step3").getStream()

    for {
      (headers, enumerator) <- stream
      elements <- enumerator &> toBytes |>>> processElements
    } yield {
      Ok.chunked(Enumerator.enumerate(elements)).withHeaders(X_HELP -> "Process elements with WS.getStream and Iteratee.repeat")
    }
  }

  def step6 = Action.async {
    val element: Future[Element] = WS.url("http://localhost:9000/step3").get[Element](_ => toBytes &>> processElement).flatMap(_.run)
    element.map(Ok(_).withHeaders(X_HELP -> "WS.get example with Future[Element]"))
  }

  def step7 = Action.async {
    val element: Future[Seq[Element]] = WS.url("http://localhost:9000/step3").get[Seq[Element]](_ => toBytes &>> processElements).flatMap(_.run)
    element.map(Ok(_).withHeaders(X_HELP -> "WS.get example with Future[Seq[Element]"))
  }

  def step8 = Action {
    import play.api.libs.EventSource.EventIdExtractor._
    import play.api.libs.EventSource.EventNameExtractor._
    implicit val elementDataEvents: EventDataExtractor[Element] = EventDataExtractor[Element] { e => Json.prettyPrint(Json.toJson(e))}
    val eventSource: Enumeratee[Element, String] = EventSource[Element]()
    val x: Iteratee[Byte, JsValue] = processElement.asJson
    Ok
  }
}