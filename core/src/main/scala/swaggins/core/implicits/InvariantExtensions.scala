package swaggins.core.implicits

import cats.Invariant

trait InvariantExtensions {
  implicit def toInvariantExtensionsOps[F[_], A](dec: F[A]): InvariantXmapOps[F, A] = new InvariantXmapOps(dec)
}

class InvariantXmapOps[F[_], A](val dec: F[A]) extends AnyVal {
  final def xmap[B](f: A => B, g: B => A)(implicit F: Invariant[F]): F[B] = F.imap(dec)(f)(g)
}
