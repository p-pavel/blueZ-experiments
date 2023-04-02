package com.perikov.dbus

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.implicits.*
import scala.deriving.Mirror

enum Endianess(val value: Byte):
  case Little extends Endianess('l'.toByte)
  case Big extends Endianess('B'.toByte)

enum MessageType(val value: Byte):
  case Invalid extends MessageType(0)
  case MethodCall extends MessageType(1)
  case MethodReturn extends MessageType(2)
  case Error extends MessageType(3)
  case Signal extends MessageType(4)

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

case class MessageFlag(
    allowInteractiveAuthorization: Boolean = false,
    noAutoStart: Boolean = false,
    noReplyExpected: Boolean = false
)

opaque type ProtocolVersion = Byte
opaque type BodyLength = Long
opaque type Serial = Long

import scodec.{Codec, Decoder, Encoder, Attempt, Err}

val t: Decoder[HeaderFieldType] = {
  val valMap = HeaderFieldType.values.map(v => v.value -> v).toMap
  byte.emap { b =>
    Attempt.fromOption(
      valMap.get(b),
      Err.General(s"Invalid HeaderFieldType value: $b", Nil)
    )
  }
}

import scodec.codecs.*

val a = int32L
val MessageFlagCodec: Codec[MessageFlag] = (
  bool ::
    bool ::
    bool ::
    bool ::
    bool ::
    bool ::
    bool ::
    bool
).as[MessageFlag]

@main
def testCodec(): Unit =
  val u = summon[Codec[(Boolean, Boolean)]]
  println(u.encode((true, false)))
