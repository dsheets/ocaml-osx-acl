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

module Gen = Osx_acl_lwt_generated
module G = Osx_acl_bindings.C(Gen)
module C = struct
  type 'a m = 'a Lwt.t

  let free p = G.(free p).Gen.lwt
  let init k = G.(init k).Gen.lwt
  let create_entry a e = G.(create_entry a e).Gen.lwt
  let delete_entry a e = G.(delete_entry a e).Gen.lwt
  let get_entry a w e = G.(get_entry a w e).Gen.lwt
  let add_perm s p = G.(add_perm s p).Gen.lwt
  let get_perm_np s p = G.(get_perm_np s p).Gen.lwt
  let clear_perms s = G.(clear_perms s).Gen.lwt
  let delete_perm s p = G.(delete_perm s p).Gen.lwt
  let get_permset e s = G.(get_permset e s).Gen.lwt
  let set_permset e s = G.(set_permset e s).Gen.lwt
  let get_qualifier e = G.(get_qualifier e).Gen.lwt
  let get_tag_type e t = G.(get_tag_type e t).Gen.lwt
  let set_qualifier e q = G.(set_qualifier e q).Gen.lwt
  let set_tag_type e t = G.(set_tag_type e t).Gen.lwt
  let get_fd f = G.(get_fd f).Gen.lwt
  let get_file p t = G.(get_file p t).Gen.lwt
  let set_fd f a = G.(set_fd f a).Gen.lwt
  let set_file p t a = G.(set_file p t a).Gen.lwt
end
include Osx_acl.Make(Osx_membership_lwt.M)(Osx_membership_lwt)(C)
