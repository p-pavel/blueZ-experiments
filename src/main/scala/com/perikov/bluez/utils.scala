package com.perikov.bluez

import cats.effect.*

private[bluez] object utils:
  extension [A, F[_]](ra: Resource[F, A])
    def useInContext[B](f: A ?=> F[B])(using F: MonadCancelThrow[F]): F[B] =
      ra.use(a => f(using a))
end utils
