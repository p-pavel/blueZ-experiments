import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.std.*
import cats.effect.implicits.*
import scala.concurrent.duration.*
import scribe.Scribe

class Test[F[_]: Monad](using bluez: BlueZ[F], log: Scribe[F]):
  private def logDevice(d: bluez.Device) =
    d.name.flatMap(name => log.info(s"Found device: $name"))
    
  val someTest = bluez
    .scanForDevices(3.seconds)
    .flatMap(_.traverse_(logDevice))

object Test extends IOApp.Simple:
  import scribe.cats.io.*
  given Scribe[IO] = scribe.cats[IO]
  def run = BlueZ.resource[IO].useInContext {
    Test[IO].someTest
  }
