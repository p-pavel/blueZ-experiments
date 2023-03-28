import cats.*
import cats.implicits.*
import cats.effect.*
import BlueZ.UUID
import scala.concurrent.duration.FiniteDuration

extension [A, F[_]](ra: Resource[F, A])
  def useInContext[B](f: A ?=> F[B])(using F: MonadCancelThrow[F]): F[B] =
    ra.use(a => f(using a))

trait BlueZ[F[_]]:
  val devices: F[Seq[Device]]
  def scanForDevices(during: FiniteDuration): F[Seq[Device]] 

  /** The base type for things that do not have meaning outside of the device
    */
  type DeviceBound = { type Con }

  type Characteristic <: DeviceBound { type T }
  type Device <: DeviceBound
  type Service <: DeviceBound
  extension (dev: Device)
    def name: F[String]
    def services: F[Seq[Service { type Con = dev.Con }]]
    def connected[A](f: dev.Con ?=> F[A]): F[A]
  // extension (srv: Service)
  //   def getCharacteristic
  //       : F[Characteristic { type Con = srv.Con; type T = Int }]

  // extension (char: Characteristic)
  //   def read(using char.Con): F[char.T]
  //   def write(using char.Con)(data: char.T): F[Unit]

  // def example(d: Device, c2: Characteristic)(using F: Monad[F]) =
  //   import cats.implicits.*

  //   d.getService.flatMap { srv =>
  //     srv.getCharacteristic.flatMap { char =>
  //       d.connection(char.read)
  //     }
  //   }




object BlueZ:
  import com.github.hypfvieh.bluetooth
  import bluetooth.wrapper.*
  import bluetooth.*
  import cats.implicits.*
  import cats.effect.implicits.*
  import scala.jdk.CollectionConverters.*
  opaque type UUID = String
  object UUID:
    def apply(s: String): Option[UUID] = util.Try(java.util.UUID.fromString(s).toString).toOption
    given Eq[UUID] = Eq.fromUniversalEquals
    given Show[UUID] = s => s"BUUID($s)"

  def resource[F[_]](using F: Sync[F]): Resource[F, BlueZ[F]] =
    Resource
      .make(F.interruptible(DeviceManager.createInstance(false)))(dm =>
        F.interruptible(dm.closeConnection)
      )
      .map(dm =>
        new BlueZ[F]:

          extension (dev: Device)
            override def name = F.interruptible(dev.getName)
            override def services: F[Seq[Service {type Con = dev.Con}]] = 
              F.interruptible(dev.getGattServices.asScala.toSeq.asInstanceOf)
            override def connected[A](f: (dev.Con) ?=> F[A]): F[A] = ???

          val devices: F[Seq[Device]] =
            F.interruptible(dm.getDevices.asScala.toSeq.asInstanceOf)
          def scanForDevices(during: FiniteDuration): F[Seq[Device]] = F.interruptible {
            dm.scanForBluetoothDevices(during.toMillis.toInt).asScala.toSeq.asInstanceOf[Seq[Device]]
          }

          type Device = BluetoothDevice & DeviceBound
          type Service = BluetoothGattService & DeviceBound
          type Characteristic = BluetoothGattCharacteristic & DeviceBound & {type T}
      )
