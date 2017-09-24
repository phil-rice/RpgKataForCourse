package org.validoc.rpgKata

case class Lens[A, B](get: A => B, set: B => A => A) extends Immutable {
  def apply(whole: A): B = get(whole)

  def ~>[C](fn: B => C): A => C = a => fn(get(a))

  def transform(f: B => B): (A => A) = { a: A => set(f(get(a)))(a) }

  def modify[C](f: (B, C) => B): (A => C => A) = { a => c => val newB = f(get(a), c); set(newB)(a) }

  def has[T](fn: B => T): (A) => T = { a: A => fn(get(a)) }

  def compose[C](that: Lens[C, A]) = Lens[C, B](that.get andThen get, set andThen that.transform)

  def andThen[C](that: Lens[B, C]): Lens[A, C] = that compose this

  def =>=[C](that: Lens[B, C]): Lens[A, C] = andThen(that)
}