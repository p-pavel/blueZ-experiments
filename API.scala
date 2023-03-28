import cats.*
import cats.effect.*

extension [A, F[_]](ra: Resource[F, A])
  def useInContext[B](f: A ?=> F[B])(using F: MonadCancelThrow[F]): F[B] =
    ra.use(a => f(using a))

trait BlueZ[F[_]]:
  val devices: F[Seq[Device]]

  /** The base type for things that do not have meaning outside of the device
    */
  type DeviceBound = { type Con }

  type Characteristic <: DeviceBound { type T }
  type Device <: DeviceBound
  type Service <: DeviceBound
  // extension (dev: Device)
  //   def getService: F[Service { type Con = dev.Con }]
  //   def connection[A](f: dev.Con ?=> F[A]): F[A]
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

  def resource[F[_]](using F: Sync[F]): Resource[F, BlueZ[F]] =
    Resource
      .make(F.interruptible(DeviceManager.createInstance(false)))(dm =>
        F.interruptible(dm.closeConnection)
      )
      .map(dm =>
        new BlueZ[F]:
          val devices: F[Seq[Device]] =
            F.interruptible(dm.getDevices.asScala.toSeq.asInstanceOf)

          type Device = BluetoothDevice & DeviceBound
          type Service = BluetoothGattService & DeviceBound
          type Characteristic = BluetoothGattCharacteristic & DeviceBound & {type T}
      )

// object BlueZ:
//   transparent inline def apply[F[_]](using b: BlueZ[F]) = b

//   type Device <: {
//     type Service <: GATTService; type Con <: Connection;
//     type C <: Characteristic
//   }
//   type GATTService <: { type C <: Characteristic }
//   type Characteristic <: { type T }
//   type Connection <: { type C <: Characteristic }
//   type BID[T] = java.util.UUID // TODO: support short and string BIDs

//   // TODO: bind characteristics to devices

// end BlueZ

// trait Microbit:
//   import BlueZ.*
//   import java.util.UUID
//   val ledTextServie: BID[GATTService] =
//     UUID.fromString("e95dd91d-251d-470a-a062-fa1922dfa9a8")

// trait BlueZ[F[_]]:
//   import BlueZ.*

//   val devices: F[Seq[Device]]
//   extension (dev: Device)
//     def name: F[String]
//     def rssi: F[Short]
//     def gattServices: F[Seq[dev.Service { type C = dev.C }]]
//     def serviceByBID(
//         bid: BID[GATTService]
//     ): F[Option[dev.Service { type C = dev.C }]]
//     def isConnected: F[Boolean]
//     def run[A](f: Connection { type C = dev.C } ?=> A): F[A]
//   extension (srv: GATTService)
//     def characteristics: F[Seq[srv.C]]
//     def characteristicByBID(bid: BID[Characteristic]): F[Option[srv.C]]

//   extension (char: Characteristic)
//     def read(using con: Connection { type C >: char.type }): F[char.T]
//     def write(using con: Connection { type C = char.type })(
//         data: char.T
//     ): F[Unit]
