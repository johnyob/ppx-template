(* A runtime library used by code generated by the PPX *)

module Nat : sig
  type t = private int

  val of_int_unsafe : int -> t
end = struct
  type t = int

  let of_int_unsafe n = n
end
