import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.std.*
import cats.effect.implicits.*
import scala.concurrent.duration.*
import scribe.Scribe

class Test[F[_]: Monad: Parallel](using bluez: BlueZ[F], log: Scribe[F]):
  private def logDevice(d: bluez.Device) =
    d.name.flatMap(name => log.info(s"Found device: $name"))

  val someTest =
    log.info("Start scanning") *>
      bluez
        .scanForDevices(3.seconds)
        .flatMap(_.parTraverse_(logDevice))

object Test extends IOApp.Simple:
  import scribe.cats.io.*
  given log: Scribe[IO] = scribe.cats[IO]
  def run =
    log.info("Start") *>
    BlueZ.resource[IO].useInContext {
      Test[IO].someTest
    } *>
    log.info("End")
