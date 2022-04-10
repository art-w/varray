module Array_backend
: Varray_sig.ARRAY with type 'a elt = 'a
                    and type 'a t = 'a array
= struct
  include Array
  type 'a elt = 'a
  let get = Array.unsafe_get
  let set = Array.unsafe_set
  let empty : type a. unit -> a t = fun () -> [| |]
  let placeholder : type a. a elt = Obj.magic ()
  let create n = Array.make n placeholder
  let erase_at t i = set t i placeholder
end

module type ARRAY = Varray_sig.ARRAY

module type ARRAY_TYPES = sig
  type 'a array_elt
  type 'a array_t
end

module Internals (X : ARRAY_TYPES) = struct
  module type UNSAFE = Varray_sig.TIER with type 'a Array.elt = 'a X.array_elt
                                        and type 'a Array.t = 'a X.array_t
end

module type S = sig
  include Varray_sig.S
  module Backend : sig
    type 'a array_elt = 'a elt
    type 'a array_t = 'a array
  end
  module Unsafe : Internals(Backend).UNSAFE
end

module Grow (Arg : Varray_sig.TIER)
: S with type 'a elt = 'a Arg.elt and type 'a array = 'a Arg.array
= struct
  module V = Grow.Make (Arg)
  include Array_like.Make (V)
  module Backend = struct
    type 'a array_elt = 'a elt
    type 'a array_t = 'a array
  end
  module Unsafe : Internals(Backend).UNSAFE = V.Tier
end

module Make (Array : ARRAY)
: S with type 'a array = 'a Array.t and type 'a elt = 'a Array.elt
= Grow (Circular.Make (Array))

module Root (V : S)
: S with type 'a array = 'a V.array and type 'a elt = 'a V.elt
= Grow (Root.Make (V.Unsafe))

module Circular
: S with type 'a array = 'a Stdlib.Array.t and type 'a elt = 'a
= Make (Array_backend)

include Root (Circular)
