(*
 * Copyright (c) 2016 David Sheets <dsheets@docker.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module Perm : sig
  type t =
    | Read_data
    | Write_data
    | Execute
    | Delete
    | Append_data
    | Delete_child
    | Read_attributes
    | Write_attributes
    | Read_extattributes
    | Write_extattributes
    | Read_security
    | Write_security
    | Change_owner
    | Synchronize
    | Unknown of int

  val to_string : t -> string

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  val of_int : int -> t

  val to_int : t -> int
end

module Tag : sig
  type t =
    | Undefined
    | Allow
    | Deny
    | Unknown of int

  val of_int : int -> t

  val to_int : t -> int
end

module Entry : sig
  type t = {
    perms : Perm.Set.t;
    qualifier : Osx_membership.id;
    tag : Tag.t;
  }
end

type t = Entry.t list

module type S =
sig
  type _ m

  val get : string -> t option m
  val fget : Unix.file_descr -> t option m
  val set : string -> t -> unit m
  val fset : Unix.file_descr -> t -> unit m
end

module Make
    (M: Osx_membership.MONAD)
    (Membership: Osx_membership.S with module M = M)
    (C: Osx_acl_bindings.S with type 'a m = 'a M.m) :
  S with type 'a m = 'a M.m

include S with type 'a m = 'a
