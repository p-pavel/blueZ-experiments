package com.perikov.dbus

import cats.*
import cats.implicits.*
import cats.data.*
import cats.effect.*
import cats.effect.implicits.*
import scala.deriving.Mirror
import scala.runtime.TupleMirror

import scodec.{Codec, Decoder, Encoder, Attempt, Err, DecodeResult, SizeBound}
import scodec.codecs.*
import scodec.bits.*

trait DBusType[T](c: Codec[T]):
  def alingnmentBytes: Int
  def stateCodec: State[Long, Codec[T]] =
    for
      pos <- State.get[Long]
      padSize = alingnmentBytes - (pos % alingnmentBytes)
      _ <- State.set(padSize + alingnmentBytes)
    yield new Codec[T]:
      override def sizeBound = SizeBound(
        c.sizeBound.lowerBound,
        c.sizeBound.upperBound.map(_ + padSize * 8)
      )
      override def encode(value: T): Attempt[BitVector] =
        c.encode(value).map(_.padLeft(padSize * 8))
      override def decode(bits: BitVector): Attempt[DecodeResult[T]] =
        c.decode(bits.drop(padSize * 8))

object DBusType:
  import Tuple.*
  (IO(3), IO("SDf")).tupled
end DBusType

extension [T](c: Codec[T])
  def aligned(bytes: Int): DBusType[T] = new DBusType[T](c):
    override val alingnmentBytes = bytes

enum Endianess(val value: Byte, val endianess: ByteOrdering):
  case Little extends Endianess('l'.toByte, ByteOrdering.LittleEndian)
  case Big extends Endianess('B'.toByte, ByteOrdering.BigEndian)

object Endianess:
  given DBusType[Endianess] =
    mappedEnum(byte, values.map(v => v -> v.value)*).aligned(1)

enum MessageType(val value: Byte):
  case Invalid extends MessageType(0)
  case MethodCall extends MessageType(1)
  case MethodReturn extends MessageType(2)
  case Error extends MessageType(3)
  case Signal extends MessageType(4)

object MessageType:
  given DBusType[MessageType] =
    mappedEnum(byte, values.map(v => v -> v.value)*).aligned(1)

enum HeaderFieldType(val value: Byte):
  case Invalid extends HeaderFieldType(0)
  case Path extends HeaderFieldType(1)
  case Interface extends HeaderFieldType(2)
  case Member extends HeaderFieldType(3)
  case ErrorName extends HeaderFieldType(4)
  case ReplySerial extends HeaderFieldType(5)
  case Destination extends HeaderFieldType(6)
  case Sender extends HeaderFieldType(7)
  case Signature extends HeaderFieldType(8)
  case UnixFds extends HeaderFieldType(9)

object HeaderFieldType:
  given DBusType[HeaderFieldType] =
    mappedEnum(byte, values.map(v => v -> v.value)*).aligned(1)

case class MessageFlag(
    allowInteractiveAuthorization: Boolean = false,
    noAutoStart: Boolean = false,
    noReplyExpected: Boolean = false
)

opaque type ProtocolVersion = Byte
opaque type BodyLength = Long
opaque type Serial = Long

case class Message(
    endianess: Endianess,
    messageType: MessageType,
    flags: MessageFlag,
    protocolVersion: ProtocolVersion,
    bodyLength: BodyLength,
    serial: Serial,
    fields: Map[HeaderFieldType, String],
    body: IArray[Byte]
)

// import org.openjdk.jmh.annotations.*

// class TestCodec:
//   @Benchmark
//   def testCodec(): Unit =
//     val u = summon[Codec[(Boolean, Boolean)]]
//     u.encode((true, false))
