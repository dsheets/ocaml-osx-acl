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

module Basic = struct

  let missing () =
    match Osx_acl.get "nonexistent" with
    | None -> ()
    | Some _ -> Alcotest.fail "should not return ACL for nonexistent file"

  let none () =
    match Osx_acl.get "." with
    | None -> ()
    | Some _ -> Alcotest.fail "should not return ACL for ACL-less cwd"

  let uid_entry =
    let tag = Osx_acl.Tag.Allow in
    let qualifier = Osx_membership.Uid (Unix.getuid ()) in
    let perms = Osx_acl.Perm.(Set.of_list [
      Read_extattributes;
      Write_extattributes;
    ]) in
    { Osx_acl.Entry.tag; qualifier; perms }

  let gid_entry =
    let tag = Osx_acl.Tag.Allow in
    let qualifier = Osx_membership.Gid (Unix.getgid ()) in
    let perms = Osx_acl.Perm.(Set.of_list [
      Read_extattributes;
      Write_extattributes;
    ]) in
    { Osx_acl.Entry.tag; qualifier; perms }

  let set_single () =
    Osx_acl.set "." [uid_entry];
    try
      let acl = Osx_acl.get "." in
      Osx_acl.set "." [];
      if acl <> Some [uid_entry]
      then Alcotest.fail "ACLs don't match"
    with exn ->
      Osx_acl.set "." [];
      raise exn

  let set_multi () =
    Osx_acl.set "." [uid_entry; gid_entry];
    try
      let acl = Osx_acl.get "." in
      Osx_acl.set "." [];
      if acl <> Some [uid_entry; gid_entry]
      then Alcotest.fail "ACLs don't match"
    with exn ->
      Osx_acl.set "." [];
      raise exn

  let tests = [
    "missing", `Quick, missing;
    "none", `Quick, none;
    "set_single", `Quick, set_single;
    "set_multi", `Quick, set_multi;
  ]
end

let tests = [
  "Basic", Basic.tests;
]

;;
Alcotest.run "OSX acl" tests
