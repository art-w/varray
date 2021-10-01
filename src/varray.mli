(**
A varray is a {b var}iable sized {b array}, also known as a resizable or
dynamic array.

Just like an array, random access / update is {b O(1)}. But you can also grow
the varray by appending or prepending an element in constant time.
Insertion and deletion at a specific index cost {b O({%html:<sup>k</sup>%}√N)}
for any constant [k ≥ 1] of your choosing.

For convenience, the recommended complexity tradeoff between time and memory is
provided below with the constant parameter [k = 2].  You will find the internal
building blocks at the end of this documentation to create a custom Varray with
different asymptotics.
*)

include Varray_sig.S with type 'a elt = 'a
                      and type 'a array = 'a Stdlib.Array.t (** @inline *)

(** {1 Signature} *)

module Internals (X : sig type 'a array_elt type 'a array_t end) : sig
  module type UNSAFE
end
(** The signature of the internal operations, required by the {! Root} functor
    below. *)

module type S = sig
  include Varray_sig.S (** @inline *)

  (** {1 Internals} *)

  (** This part can be ignored as it exposes no user-facing functionality!..
      but the design pattern is neat.  The {! Root} functor requires access to
      internal operations, that should neither be called nor implemented by a
      user of this library.
  *)

  module Backend : sig
    type 'a array_elt = 'a elt
    type 'a array_t = 'a array
  end
  (** The ['a array] and ['a elt] types. *)

  module Unsafe : Internals(Backend).UNSAFE
  (** The internal operations, safely concealed behind an abstract signature!
  *)

  (**
      This could not have been written as:

      {[ module Unsafe : UNSAFE with type 'a array = 'a array
                                 and type 'a elt = 'a elt ]}

      Since the signature [UNSAFE] can't be type constrained without also
      having all its internal functions be public. The {! Internals} functor
      circumvent this rule by exposing an opaque signature parametrized by
      the type constraints.  *)
end
(** The signature of a varray. *)

(** {1 Build your own} *)

(** The family of varrays is defined by calling the {! Root} functor as many
   times as required: *)

(**

{%html:
  <style>
  th, td {
    padding: 0.5em 1em;
    border-left: 1px solid #CDD7E2; /* #E1ECFB; */
  }

  tbody tr:nth-child(odd) { background: #E1ECFB }
  tr th:first-child { border: none }

  </style>
  <table>
    <thead>
    <tr>
      <th>Module</th>
      <th><code>get</code>, <code>set</code>
          <br/>
          <code>push</code>, <code>pop</code></th>
      <th><code>insert_at</code><br/><code>delete_at</code></th>
      <th>Memory overhead</th>
    </tr>
    </thead>
    <tbody>
      <tr>
        <th><code>Circular</code></th>
        <td>O(1)</td>
        <td>O(N)</td>
        <td>O(N)</td>
      </tr>
      <tr>
        <th><code>Root (Circular)</code></th>
        <td>O(1)</td>
        <td>O(<sup>2</sup>√N)</td>
        <td>O(<sup>2</sup>√N)</td>
      </tr>
      <tr>
        <th><code>Root (Root (Circular))</code></th>
        <td>O(1)</td>
        <td>O(<sup>3</sup>√N)</td>
        <td>O(N<sup>2/3</sup>)</td>
      </tr>
      <tr>
        <th><code>Root<sup>k-1</sup> (Circular)</code></th>
        <td>O(k)</td>
        <td>O(k<sup>2</sup> × <sup>k</sup>√N)</td>
        <td>O(N<sup>k-1 / k</sup>)</td>
      </tr>
    </tbody>
  </table>
%}

*)

(** The first step is to choose a backend [Array] that will be used to store
    the elements: *)

module type ARRAY = sig
  include Varray_sig.ARRAY (** @inline *)
end

(** Some good candidates from the standard library are [Array] for polymorphic
    values, [BigArray.Array1] for numbers, [Bytes] for characters,
    or [Weak] for weak pointers. *)

module Make (Array : ARRAY)
: S with type 'a array = 'a Array.t and type 'a elt = 'a Array.elt
(** [Make (Array)] returns a circular varray using [Array] as its backend.
    The resulting varray has parameter [k = 1], meaning that [push] and [pop]
    at both ends is O(1) but random insertion and deletions are O(N).  *)

module Circular : S with type 'a array = 'a Array.t and type 'a elt = 'a

(** [Circular] is a predefined circular varray using a polymorphic
    [Stdlib.Array]. *)

module Root (V : S) : S with type 'a array = 'a V.array
                         and type 'a elt = 'a V.elt
(** [Root (Varray)] nests an existing [Varray] to improve the performances of
    random insertion and deletion.
    However, it does so at the price that random access and insertion and
    deletion at the ends will be a constant time slower!  *)
