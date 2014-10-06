package controllers

import org.specs2.mutable._
import play.api.libs.iteratee._

import scala.concurrent.Future

class IterateeSpec extends Specification {

  import controllers.Application._

  "The sum iteratee" should {
    "calculate a correct sum of one element" in {

      val input = Element(1)

      val result: Future[Int] = sum.feed(Input.El(input)).flatMap(_.run)
      result must beEqualTo(1).await
    }

    "calculate zero for an EOF stream" in {
      val result: Future[Int] = sum.feed(Input.EOF).flatMap(_.run)
      result must beEqualTo(0).await
    }


    "calculate a correct sum of multiple elements (v1)" in {

      val input1 = Element(1)
      val input2 = Element(2)

      val result = {
        for {
          firstStep <- sum.feed(Input.El(input1))
          secondStep <- firstStep.feed(Input.El(input2))
        } yield secondStep
      }.flatMap(_.run)
      result must beEqualTo(3).await
    }

    "calculate a correct sum of multiple elements (v2)" in {
      val result: Future[Int] = (Enumerator(Element(1), Element(2)) |>>> sum)
      result must beEqualTo(3).await
    }
  }

  "The findUntil thingie" should {
    "do its thing"  in  {
      val testEnumerator: Enumerator[Byte] = Enumerator.enumerate("Hello darling".getBytes.toList)
      val x: Future[Iteratee[Byte, List[Byte]]] = testEnumerator |>> findChar('d').flatMap(e => Iteratee.getChunks[Byte])
      val result = x.flatMap(_.run)
      result must beEqualTo("darling".getBytes().toList).await

    }
  }

}
