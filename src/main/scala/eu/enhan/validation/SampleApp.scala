package eu.enhan.validation
import java.io.IOException

import cats.data._
import cats.implicits._
import scalaz.zio.console._
import scalaz.zio.{App, ZIO}

/**
  * @author Emmanuel Nhan
  */
object SampleApp extends App {
  override def run(args: List[String]) = loop().fold(_ => 1, _ => 0)


  val validation = Kleisli{ raw: (String, String, String) =>
    val v: ValidatedNel[AlertCreationError, ValidationLogic.Alert] = validateInput(raw._1, raw._2, raw._3).andThen { payload => ValidationLogic.validate(payload)}
    val strToDisplay: String = v.fold(_.map(_.toString).reduceLeft{ (acc, e) => s"$acc,$e"}, s => s.toString )
    putStrLn(strToDisplay)
  }

  private val appLoop = for {
    _ <- putStrLn("Name ?")
    name <- getStrLn
    _ <- putStrLn("TA?")
    tA <- getStrLn
    _ <- putStrLn("TC?")
    tC <- getStrLn
    _ <- validation((name, tA, tC))
  } yield (
  )


  private def loop(): ZIO[Console, IOException, Unit] ={
    val shallContinue: ZIO[Console, IOException, String] = for {
      _ <- appLoop
      _ <- putStrLn("Continue? [y/N]")
      input <- getStrLn
    } yield input
    shallContinue.flatMap {
      case "y" => loop()
      case _ => ZIO.unit
    }
  }

  def validateInput(name: String, tA: String, tC: String): ValidatedNel[AlertCreationError, AlertPayload] = {

    def safeParseInt(s: String): ValidatedNel[AlertCreationError, Int] = Validated.condNel(s.matches("-?[0-9]+"), s.toInt, AlertCreationError.ParseError(s"$s is not an int"))

    (Validated.validNel(name), safeParseInt(tA), safeParseInt(tC)).mapN(AlertPayload)

  }



}
