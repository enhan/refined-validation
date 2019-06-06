package eu.enhan.validation

import cats.data._
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.{MaxSize, NonEmpty}
import eu.timepit.refined.numeric.Interval.{Closed, OpenClosed}



final case class AlertPayload(name: String, tA: Int, tC: Int)

abstract sealed class AlertCreationError

object AlertCreationError {

  final case class ParseError(msg: String) extends AlertCreationError
  final case object InvalidName extends AlertCreationError
  final case object InvalidTA extends  AlertCreationError
  final case object InvalidTC extends  AlertCreationError

}



/**
  *
  */
object ValidationLogic {

  type Name = And[NonEmpty, MaxSize[W.`5`.T]]
  type AlertName = String Refined Name

  type TAP = OpenClosed[W.`100`.T, W.`1000`.T]
  type TCP = Closed[W.`100`.T, W.`1000`.T]

  type TA = Int Refined TAP
  type TC = Int Refined TCP


  final case class Alert(name: AlertName, tA: TA, tC: TC)


  def validate(alertPayload: AlertPayload): ValidatedNel[AlertCreationError, Alert] = {

    val nameValidation: Validated[AlertCreationError, AlertName] = Validated.fromEither(
      refineV[Name](alertPayload.name)
    ).leftMap( _ => AlertCreationError.InvalidName)

    val tAValidation: Validated[AlertCreationError, TA] = Validated.fromEither(
      refineV[TAP](alertPayload.tA)
    ).leftMap( _ => AlertCreationError.InvalidTA)

    val tCvalidation: Validated[AlertCreationError, TC] = Validated.fromEither(
      refineV[TCP](alertPayload.tC)
    ).leftMap(_ => AlertCreationError.InvalidTC)

    (nameValidation.toValidatedNel,
      tAValidation.toValidatedNel,
      tCvalidation.toValidatedNel
    ).mapN(Alert)

  }



}
