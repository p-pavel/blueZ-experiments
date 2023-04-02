package com.perikov.dbus

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.data.*
import cats.effect.implicits.*
import scribe.Scribe
import fs2.io.net.unixsocket.*
import fs2.io.net.Socket

/** Primitive SASL authentication for DBus. */
class DBusSASL[F[_]: Monad](using log: Scribe[F]):
  import log.*

  import fs2.Chunk
  private def toHex(n: Int): String =
    n.toString.getBytes.map(_.toInt.toHexString).mkString

  type Error = String
  def authenticate(sock: Socket[F], uid: Int): F[Option[Error]] =
    def sendString(s: String) =
      sock.write(Chunk.array((s + "\r\n").getBytes)) *> trace(
        s"Sent: $s"
      )
    def sendCmd(s: String) =
      sendString(s) *> OptionT(sock.read(1024))
        .map(chunk => String(chunk.toArray))
        .value
        .flatTap(s => trace(s"Received: $s"))

    for // TODO: Check for errors
      _ <- trace("Sending 0")
      _ <- sock.write(Chunk(0.toByte))
      _ <- sendCmd("AUTH EXTERNAL " + toHex(uid))
      _ <- sendCmd("NEGOTIATE_UNIX_FD")
      _ <- sendString("BEGIN")
    yield None

object SASL extends IOApp.Simple:
  given log: Scribe[IO] = scribe.cats.io
  val address = UnixSocketAddress("/var/run/dbus/system_bus_socket")
  val sockResource = fs2.io.net.unixsocket.UnixSockets[IO].client(address)

  def run: IO[Unit] = sockResource.use { sock =>
    DBusSASL[IO]
      .authenticate(sock, 1001)
      .flatMap { // TODO: Get uid from somewhere
        case Some(err) => IO(println(s"Error: $err"))
        case None      => IO(println("Success"))
      }
  }
