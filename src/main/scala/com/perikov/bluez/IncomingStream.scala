package com.perikov.bluez

import cats.effect.*
import cats.implicits.*
import fs2.Stream

object IncomingQueue:
  def resource[F[_]: Async, T](
      queueSize: Int
  ): Resource[F, (std.Queue[F, T], T => Unit)] =
    (
      std.Dispatcher.sequential[F],
      Resource.eval(std.Queue.bounded[F, T](queueSize))
    ).tupled.map { (dispatcher, queue) =>
      (
        queue,
        queue.offer andThen dispatcher.unsafeRunSync
      )
    }
end IncomingQueue

class IncomingStream[F[_], T](val stream: Stream[F, T], val put: T => Unit)

object IncomingStream:
  def resource[F[_]: Async, T](
      queueSize: Int
  ): Resource[F, IncomingStream[F, T]] =
    IncomingQueue
      .resource[F, T](queueSize)
      .map((queue, put) =>
        new IncomingStream(Stream.fromQueueUnterminated(queue), put)
      )
