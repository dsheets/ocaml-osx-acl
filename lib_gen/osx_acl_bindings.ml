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

open Ctypes

let (|||) = (lor)

let (??>) flag int = if flag then int else 0

let (??<) field int = field land int <> 0

module Type = Osx_acl_types.C(Osx_acl_types_detected)

module type S = sig
  type 'a m

  val free : unit ptr -> (int * Signed.sint) m

  val init : int -> (unit ptr * Signed.sint) m

  val create_entry : unit ptr ptr -> unit ptr ptr -> (int * Signed.sint) m

  val delete_entry : unit ptr -> unit ptr -> (int * Signed.sint) m

  val get_entry : unit ptr -> int -> unit ptr ptr -> (int * Signed.sint) m

  val add_perm : unit ptr -> int -> (int * Signed.sint) m

  val get_perm_np : unit ptr -> int -> (int * Signed.sint) m

  val clear_perms : unit ptr -> (int * Signed.sint) m

  val delete_perm : unit ptr -> int -> (int * Signed.sint) m

  val get_permset : unit ptr -> unit ptr ptr -> (int * Signed.sint) m

  val set_permset : unit ptr -> unit ptr -> (int * Signed.sint) m

  val get_qualifier : unit ptr -> (unit ptr * Signed.sint) m

  val get_tag_type : unit ptr -> int ptr -> (int * Signed.sint) m

  val set_qualifier : unit ptr -> unit ptr -> (int * Signed.sint) m

  val set_tag_type : unit ptr -> int -> (int * Signed.sint) m

  val get_fd : int -> (unit ptr * Signed.sint) m

  val get_file : string -> int -> (unit ptr * Signed.sint) m

  val set_fd : int -> unit ptr -> (int * Signed.sint) m

  val set_file : string -> int -> unit ptr -> (int * Signed.sint) m
end

module C(F: Cstubs.FOREIGN) = struct

  let free = F.(foreign "acl_free" (ptr void @-> returning int))

  let init = F.(foreign "acl_init" (int @-> returning Type.t))

  let create_entry = F.(foreign "acl_create_entry" (
    ptr Type.t @-> ptr Type.entry @-> returning int
  ))

  let delete_entry = F.(foreign "acl_delete_entry" (
    Type.t @-> Type.entry @-> returning int
  ))

  let get_entry = F.(foreign "acl_get_entry" (
    Type.t @-> int @-> ptr Type.entry @-> returning int
  ))

  let add_perm = F.(foreign "acl_add_perm" (
    Type.permset @-> Type.Perm.t @-> returning int
  ))

  let get_perm_np = F.(foreign "acl_get_perm_np" (
    Type.permset @-> Type.Perm.t @-> returning int
  ))

  let clear_perms = F.(foreign "acl_clear_perms" (
    Type.permset @->  returning int
  ))

  let delete_perm = F.(foreign "acl_delete_perm" (
    Type.permset @-> Type.Perm.t @-> returning int
  ))

  let get_permset = F.(foreign "acl_get_permset" (
    Type.entry @-> ptr Type.permset @-> returning int
  ))

  let set_permset = F.(foreign "acl_set_permset" (
    Type.entry @-> Type.permset @-> returning int
  ))

  let get_qualifier = F.(foreign "acl_get_qualifier" (
    Type.entry @-> returning (ptr void)
  ))

  let get_tag_type = F.(foreign "acl_get_tag_type" (
    Type.entry @-> ptr Type.Tag.t @-> returning int
  ))

  let set_qualifier = F.(foreign "acl_set_qualifier" (
    Type.entry @-> ptr void @-> returning int
  ))

  let set_tag_type = F.(foreign "acl_set_tag_type" (
    Type.entry @-> Type.Tag.t @-> returning int
  ))

  let get_fd = F.(foreign "acl_get_fd" (int @-> returning Type.t))

  let get_file = F.(foreign "acl_get_file" (
    string @-> Type.type_ @-> returning Type.t
  ))

  let set_fd = F.(foreign "acl_set_fd" (
    int @-> Type.t @-> returning int
  ))

  let set_file = F.(foreign "acl_set_file" (
    string @-> Type.type_ @-> Type.t @-> returning int
  ))
end
