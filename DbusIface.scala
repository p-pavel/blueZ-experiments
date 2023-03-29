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
  class DbusIfaceImpl[F[_]: Sync](bus: DBusConnection):
    def findNodes(name: Source, path: Path): F[Seq[String]] =
      remoteObject[Introspectable](name, path)
        .map(_.Introspect())
        .map(parseXML)
        .map(xml =>
          (xml \ "node").map(_ \@ "name")
        ) // TODO: check multiple nodes

    def remoteObject[A <: DBusInterface](name: Source, path: Path)(using
        c: ClassTag[A]
    ): F[A] =
      Sync[F].interruptible(
        bus.getRemoteObject(name, path, c.runtimeClass.asInstanceOf)
      )

  end DbusIfaceImpl

  def system[F[_]: Sync]: Resource[F, DbusIfaceImpl[F]] =
    Resource
      .make(
        Sync[F].interruptible(DBusConnection.getConnection(DBusBusType.SYSTEM))
      )(bus => Sync[F].interruptible(bus.disconnect()))
      .map(DbusIfaceImpl(_))

  def run: IO[Unit] =
    import scribe.cats.*
    given log: scribe.Scribe[IO] = scribe.Logger.root.f
    log.info("Starting") *>
      system[IO].use { iface =>

        val source = "org.bluez"
        val bluezPath = "/org/bluez"
        log.info("Getting nodes") *>
          iface
            .findNodes(source, bluezPath)
            .flatMap(nodes =>
              log.info("Got nodes") *>
                nodes
                  .map(n =>
                    iface
                      .remoteObject[Adapter1](source, bluezPath + "/" + n)
                      .flatMap(a => log.info(a.toString))
                  )
                  .parSequence_
            )
      }

end DbusIface
