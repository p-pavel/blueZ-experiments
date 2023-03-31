package com.perikov.bluez

import scala.jdk.CollectionConverters.*
import com.github.hypfvieh.bluetooth
import bluetooth.wrapper.BluetoothDevice
import bluetooth.DeviceManager
import org.freedesktop.dbus
import dbus.handlers.{
  AbstractPropertiesChangedHandler,
  AbstractSignalHandlerBase
}
import dbus.types.Variant
import dbus.interfaces.Properties.PropertiesChanged
import dbus.interfaces.ObjectManager.{InterfacesAdded, InterfacesRemoved}
import dbus.messages.DBusSignal
import com.github.hypfvieh.DbusHelper
import dbus.connections.impl.DBusConnection
import DBusConnection.DBusBusType
import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.implicits.*
import dbus.connections.impl.DBusConnection.DBusBusType
import dbus.interfaces.{Introspectable, DBusInterface}
import org.bluez.Adapter1

import scala.reflect.ClassTag
import fs2.Stream
import org.freedesktop.dbus.DBusPath

trait DbusIface[F[_]] {}

object DbusIface extends IOApp.Simple:
  import scala.xml.*
  private val parserFactory = javax.xml.parsers.SAXParserFactory.newInstance()
  parserFactory.setValidating(false)
  parserFactory.setFeature(
    "http://apache.org/xml/features/nonvalidating/load-dtd-grammar",
    false
  )
  parserFactory.setFeature(
    "http://apache.org/xml/features/nonvalidating/load-external-dtd",
    false
  )



  private def parseXML(s: String): Elem =
    XML.loadXML(Source.fromString(s), parserFactory.newSAXParser())

  opaque type Source = String
  opaque type Path = String

  class DbusIfaceImpl[F[_]](bus: DBusConnection)(using F: Async[F]):
    opaque type Adapter =
      Adapter1 // TODO: Adapter can't be used outside of connection
    private val source = "org.bluez"
    private val bluezPath = "/org/bluez"

    /** If name does not exist you will give invalid object
      */
    def adapter(name: String): F[Adapter] =
      remoteObject[Adapter1](source, bluezPath + "/" + name)

    val adapterNames: F[Seq[String]] =
      findNodes(source, bluezPath)

    def signalStream[T <: DBusSignal: ClassTag] =
      class Handler(action: T => Unit) extends AbstractSignalHandlerBase[T]:
        override def getImplementationClass(): Class[T] =
          summon[ClassTag[T]].runtimeClass.asInstanceOf
        override def handle(signal: T): Unit = action(signal)

        def subscribe: F[this.type] =
          F.delay(bus.addSigHandler(getImplementationClass(), this)).as(this)
        def unsubscribe: F[Unit] =
          F.delay(bus.removeSigHandler(getImplementationClass(), this))
      end Handler
      def mkHandler(action: T => Unit): Resource[F, Handler] =
        Resource.make(Handler(action).subscribe)(_.unsubscribe)

      val incomingStreamResource = IncomingStream
        .resource[F, T](10)
        .flatMap { in => mkHandler(in.put).as(in.stream) }

      Stream.resource(incomingStreamResource).flatten

    end signalStream

    def findNodes(name: Source, path: Path): F[Seq[String]] =
      remoteObject[Introspectable](name, path)
        .map(_.Introspect())
        .map(parseXML)
        .map(xml =>
          println(xml)
          (xml \ "node").map(_ \@ "name")
        ) // TODO: check multiple nodes

    def remoteObject[A <: DBusInterface](name: Source, path: Path)(using
        c: ClassTag[A]
    ): F[A] =
      F.delay(
        bus.getRemoteObject(
          name,
          path,
          c.runtimeClass.asInstanceOf,
          false /*autostart*/
        )
      )

  end DbusIfaceImpl

  def system[F[_]: Async]: Resource[F, DbusIfaceImpl[F]] =
    Resource
      .make(
        Sync[F].blocking(DBusConnection.getConnection(DBusBusType.SYSTEM))
      )(bus => Sync[F].blocking(bus.disconnect()))
      .map(DbusIfaceImpl(_))
  type T = dbus.interfaces.ObjectManager
  def run: IO[Unit] =
    import scribe.cats.*
    given log: scribe.Scribe[IO] = scribe.Logger.root.f
    log.info("Starting") *>
      system[IO].use { iface =>
        def signalStream[T <: DBusSignal: ClassTag] =
          iface.signalStream[T].evalMap { signal =>
            log.info(s"Signal: $signal")
          }
        for
          _ <- log.info("Getting nodes")
          nodes <- iface.adapterNames
          _ <- log.info("Got nodes")
          _ <- nodes
            .map(n => iface.adapter(n).flatMap(a => 
              log.info(a.toString)))
            .sequence_
          _ <- Stream
            .emits(
              Seq(
                signalStream[PropertiesChanged],
                signalStream[InterfacesAdded],
                signalStream[InterfacesRemoved]
              )
            )
            .parJoinUnbounded
            .compile
            .drain
        yield ()

      }

end DbusIface
