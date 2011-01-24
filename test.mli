type t
type u = A of t

val x : u

type v = B of w
and w

val y : v

module M : sig
  type t
  val x : t
end

type a = M.t
val z : a

module N : sig
  type l
  module P : sig
    type t = W of l
  end
end

val w : N.P.t
