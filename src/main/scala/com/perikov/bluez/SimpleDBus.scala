package com.perikov.bluez

import cats.effect.*
import cats.implicits.*

import scala.reflect.ClassTag

import org.freedesktop.dbus
import dbus.interfaces.{DBusInterface, DBusSigHandler, Introspectable, ObjectManager}
import dbus.handlers.AbstractSignalHandlerBase
import dbus.messages.DBusSignal
import dbus.connections.impl.DBusConnection
import DBusConnection.DBusBusType
import ObjectManager.InterfacesAdded


trait SimpleDBus[F[_]]:
  import SimpleDBus.*
  def children(i: Introspectable): F[Seq[Path]]
  def remoteObject[T <: DBusInterface: ClassTag](
      source: Source,
      path: Path
  ): F[T]
  def subscribeToSignal[T <: DBusSignal: ClassTag](
      process: T => F[Unit]
  ): Resource[F, Unit]
end SimpleDBus

extension (i: Introspectable)
  def children[F[_]](using s: SimpleDBus[F]): F[Seq[SimpleDBus.Path]] =
    s.children(i)

object SimpleDBus:
  import scala.util.matching.*
  //TODO: checks for names: https://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-names
  opaque type Source = String
  object Source:
    def fromString(s: String): Either[String, Source] = 
      if s.isEmpty then Left("Source can't be empty")
      else Right(s)

  opaque type Path = String
  object Path:
    def fromString(s: String): Either[String, Path] = // TODO: check path
      if s.isEmpty then Left("Path can't be empty")
      else Right(s)

    extension (p: Path) def /(s: String): Path = s"$p/$s" // TODO check string


  def system[F[_]](using F: Async[F]): Resource[F, SimpleDBus[F]] =
    Resource
      .make(
        Sync[F].blocking(DBusConnection.getConnection(DBusBusType.SYSTEM))
      )(bus => Sync[F].blocking(bus.disconnect()))
      .map(bus =>
        new SimpleDBus[F]:
          def children(i: Introspectable): F[Seq[Path]] =
            F.blocking(i.Introspect())
              .map(parseXML)
              .map(xml =>
                (xml \ "node")
                  .map(_ \@ "name")
                  .map(p => i.getObjectPath + "/" + p)
              )

          override def remoteObject[T <: DBusInterface: ClassTag](
              source: Source,
              path: Path
          ): F[T] = F.blocking(bus.getRemoteObject(source, path, clazz[T]))

          override def subscribeToSignal[T <: DBusSignal: ClassTag](
              process: T => F[Unit]
          ): Resource[F, Unit] =
            std.Dispatcher.parallel[F].flatMap { dispatcher =>

              val h = new DBusSigHandler[T] {
                def handle(signal: T): Unit =
                  dispatcher.unsafeRunSync(process(signal))
              }

              Resource.make(
                F.blocking(bus.addSigHandler(clazz, h))
              )(_ => F.blocking(bus.removeSigHandler(clazz, h)))
            }

          private def clazz[T](using c: ClassTag[T]): Class[T] =
            c.runtimeClass.asInstanceOf
      )

  import scala.xml.*
  private def parseXML(s: String): Elem =
    XML.loadXML(scala.xml.Source.fromString(s), parserFactory.newSAXParser())

  private lazy val parserFactory = {
    val res = javax.xml.parsers.SAXParserFactory.newInstance()
    res.setValidating(false)
    res.setFeature(
      "http://apache.org/xml/features/nonvalidating/load-dtd-grammar",
      false
    )
    res.setFeature(
      "http://apache.org/xml/features/nonvalidating/load-external-dtd",
      false
    )
    res
  }

end SimpleDBus

object TestSimpleBus extends IOApp.Simple:
  import SimpleDBus.*
  val dest = Source
    .fromString("org.bluez")
    .getOrElse(throw new Exception("Invalid source"))
  val root =
    Path.fromString("/org/bluez").getOrElse(throw new Exception("Invalid path"))
  def run =
    SimpleDBus.system[IO].use { bus =>
      given SimpleDBus[IO] = bus
      for
        _ <- IO.println("Children:")
        obj <- bus.remoteObject[Introspectable](dest, root)
        nodes <- obj.children
        children <- nodes
          .map(bus.remoteObject[Introspectable](dest, _))
          .sequence
        _ <- children.map(IO.println).sequence_
        _ <- IO.println("Subscribe to signal:")
        _ <- bus
          .subscribeToSignal[InterfacesAdded] { signal =>
            IO.println(s"  $signal")
          }
          .use { _ =>
            IO.never
          }
      yield ()
    }
