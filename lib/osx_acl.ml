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

module Types = Osx_acl_types.C(Osx_acl_types_detected)

let int_of_fd = Unix_representations.int_of_file_descr

module Perm = struct
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

  let to_string = function
    | Read_data -> "read"
    | Write_data -> "write"
    | Execute -> "execute"
    | Delete -> "delete"
    | Append_data -> "append"
    | Delete_child -> "delete_child"
    | Read_attributes -> "readattr"
    | Write_attributes -> "writeattr"
    | Read_extattributes -> "readextattr"
    | Write_extattributes -> "writeextattr"
    | Read_security -> "readsecurity"
    | Write_security -> "writesecurity"
    | Change_owner -> "chown"
    | Synchronize -> "sync" (* TODO: ? *)
    | Unknown x -> "unknown("^string_of_int x^")"

    let compare x y = match x, y with
      | Read_data, Read_data ->  0
      | Read_data, _         -> -1
      | _,         Read_data ->  1
      | Write_data, Write_data ->  0
      | Write_data, _          -> -1
      | _, Write_data          ->  1
      | Execute, Execute ->  0
      | Execute, _       -> -1
      | _, Execute       ->  1
      | Delete, Delete ->  0
      | Delete, _      -> -1
      | _, Delete      ->  1
      | Append_data, Append_data ->  0
      | Append_data, _           -> -1
      | _, Append_data           ->  1
      | Delete_child, Delete_child ->  0
      | Delete_child, _            -> -1
      | _, Delete_child            ->  1
      | Read_attributes, Read_attributes ->  0
      | Read_attributes, _               -> -1
      | _, Read_attributes               ->  1
      | Write_attributes, Write_attributes ->  0
      | Write_attributes, _                -> -1
      | _, Write_attributes                ->  1
      | Read_extattributes, Read_extattributes ->  0
      | Read_extattributes, _                  -> -1
      | _, Read_extattributes                  ->  1
      | Write_extattributes, Write_extattributes ->  0
      | Write_extattributes, _                   -> -1
      | _, Write_extattributes                   ->  1
      | Read_security, Read_security ->  0
      | Read_security, _             -> -1
      | _, Read_security             ->  1
      | Write_security, Write_security ->  0
      | Write_security, _              -> -1
      | _, Write_security              ->  1
      | Change_owner, Change_owner ->  0
      | Change_owner, _            -> -1
      | _, Change_owner            ->  1
      | Synchronize, Synchronize ->  0
      | Synchronize, _           -> -1
      | _, Synchronize           ->  1
      | Unknown x, Unknown y -> compare x y

    module Set = Set.Make(struct
        type nonrec t = t
        let compare = compare
      end)

    let all = [
      Read_data;
      Write_data;
      Execute;
      Delete;
      Append_data;
      Delete_child;
      Read_attributes;
      Write_attributes;
      Read_extattributes;
      Write_extattributes;
      Read_security;
      Write_security;
      Change_owner;
      Synchronize;
    ]

    let of_int = Types.Perm.(function
      | x when x = read_data -> Read_data
      | x when x = write_data -> Write_data
      | x when x = execute -> Execute
      | x when x = delete -> Delete
      | x when x = append_data -> Append_data
      | x when x = delete_child -> Delete_child
      | x when x = read_attributes -> Read_attributes
      | x when x = write_attributes -> Write_attributes
      | x when x = read_extattributes -> Read_extattributes
      | x when x = write_extattributes -> Write_extattributes
      | x when x = read_security -> Read_security
      | x when x = write_security -> Write_security
      | x when x = change_owner -> Change_owner
      | x when x = synchronize -> Synchronize
      | x -> Unknown x
    )

    let to_int = Types.Perm.(function
      | Read_data -> read_data
      | Write_data -> write_data
      | Execute -> execute
      | Delete -> delete
      | Append_data -> append_data
      | Delete_child -> delete_child
      | Read_attributes -> read_attributes
      | Write_attributes -> write_attributes
      | Read_extattributes -> read_extattributes
      | Write_extattributes -> write_extattributes
      | Read_security -> read_security
      | Write_security -> write_security
      | Change_owner -> change_owner
      | Synchronize -> synchronize
      | Unknown x -> x
    )
end

module Tag = struct
  type t =
    | Undefined
    | Allow
    | Deny
    | Unknown of int

  let of_int = Types.Tag.(function
    | x when x = undefined -> Undefined
    | x when x = allow -> Allow
    | x when x = deny -> Deny
    | x -> Unknown x
  )

  let to_int = Types.Tag.(function
    | Undefined -> undefined
    | Allow -> allow
    | Deny -> deny
    | Unknown x -> x
  )
end

module Entry = struct
  type t = {
    perms : Perm.Set.t;
    qualifier : Osx_membership.Id.t;
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
    (C: Osx_acl_bindings.S with type 'a m = 'a M.m) =
struct
  open M

  type 'a m = 'a M.m
  
  let get_file path =
    C.get_file path Types.type_extended
    >>= fun (acl, errno) ->
    if ptr_compare null acl <> 0
    then return (Some acl)
    else match Errno.of_code ~host:Errno_unix.host errno with
      | errs when List.mem Errno.ENOENT errs -> return None
      | _ -> raise_errno ~call:"acl_get_file" ~label:path errno

  let get_fd fd =
    C.get_fd fd
    >>= fun (acl, errno) ->
    if ptr_compare null acl <> 0
    then return (Some acl)
    else match Errno.of_code ~host:Errno_unix.host errno with
      | errs when List.mem Errno.ENOENT errs -> return None
      | _ -> raise_errno ~call:"acl_get_fd" errno

  let free acl =
    C.free acl
    >>= fun (rc, errno) ->
    if rc < 0
    then raise_errno ~call:"acl_free" errno
    else return ()

  let perms_of_permset permset =
    List.fold_left (fun m_set perm ->
      m_set >>= fun set ->
      C.get_perm_np permset (Perm.to_int perm)
      >>= fun (rc, errno) ->
      if rc < 0
      then raise_errno ~call:"acl_get_perm_np" errno
      else if rc = 1
      then return (Perm.Set.add perm set)
      else return set
    ) (return Perm.Set.empty) Perm.all

  let get_permset entry =
    let permset_p = allocate_n ~count:1 Types.permset in
    C.get_permset entry permset_p
    >>= fun (rc, errno) ->
    if rc < 0
    then raise_errno ~call:"acl_get_permset" errno
    else return (!@ permset_p)

  let qualifier_of_entry entry =
    C.get_qualifier entry
    >>= fun (q, errno) ->
    if ptr_compare null q = 0
    then raise_errno ~call:"acl_get_qualifier" errno
    else
      let open Membership in
      let uuid = Uuid.of_ptr q in
      Id.of_uuid uuid

  let tag_of_entry entry =
    let typ_p = allocate_n ~count:1 Types.Tag.t in
    C.get_tag_type entry typ_p
    >>= fun (rc, errno) ->
    if rc < 0
    then raise_errno ~call:"acl_get_tag_type" errno
    else return (match !@ typ_p with
      | x when x = Types.Tag.undefined -> Tag.Undefined
      | x when x = Types.Tag.allow -> Tag.Allow
      | x when x = Types.Tag.deny -> Tag.Deny
      | x -> Tag.Unknown x
    )

  let get_entries acl =
    let entry_p = allocate_n ~count:1 Types.entry in
    let rec cons_next_entry entry_id entries =
      C.get_entry acl entry_id entry_p
      >>= fun (rc, _errno) ->
      if rc = 0 then
        let entry = !@ entry_p in
        get_permset entry
        >>= fun permset ->
        perms_of_permset permset
        >>= fun perms ->
        qualifier_of_entry entry
        >>= fun qualifier ->
        tag_of_entry entry
        >>= fun tag ->
        let entries = Entry.{ perms; qualifier; tag }::entries in
        cons_next_entry Types.Entry_id.next entries
      else return (List.rev entries)
    in
    cons_next_entry Types.Entry_id.first []

  let get path =
    get_file path
    >>= function
    | None -> return None
    | Some acl ->
      catch (fun () -> get_entries acl) (fun exn ->
        free acl
        >>= fun () ->
        fail exn
      )
      >>= fun entries ->
      free acl
      >>= fun () ->
      return (Some entries)

  let fget fd =
    get_fd (int_of_fd fd)
    >>= function
    | None -> return None
    | Some acl ->
      catch (fun () -> get_entries acl) (fun exn ->
        free acl
        >>= fun () ->
        fail exn
      )
      >>= fun entries ->
      free acl
      >>= fun () ->
      return (Some entries)

  let set_file path acl_p =
    C.set_file path Types.type_extended (!@ acl_p)
    >>= fun (r, errno) ->
    if r < 0
    then raise_errno ~call:"acl_set_file" ~label:path errno
    else return ()

  let set_fd fd acl_p =
    C.set_fd fd (!@ acl_p)
    >>= fun (r, errno) ->
    if r < 0
    then raise_errno ~call:"acl_set_fd" errno
    else return ()

  let init count =
    C.init count
    >>= fun (acl, errno) ->
    if ptr_compare null acl = 0
    then raise_errno ~call:"acl_init" errno
    else return (allocate (ptr void) acl)

  let create_entry acl_p =
    let entry_p = allocate_n ~count:1 Types.entry in
    C.create_entry acl_p entry_p
    >>= fun (r, errno) ->
    if r < 0
    then raise_errno ~call:"acl_create_entry" errno
    else return entry_p

  let clear_perms permset =
    C.clear_perms permset
    >>= fun (r, errno) ->
    if r < 0
    then raise_errno ~call:"acl_clear_perms" errno
    else return ()

  let add_perm permset perm =
    C.add_perm permset (Perm.to_int perm)
    >>= fun (r, errno) ->
    if r < 0
    then raise_errno ~call:"acl_add_perm" errno
    else return ()

  let set_tag_type entry tag =
    C.set_tag_type entry (Tag.to_int tag)
    >>= fun (r, errno) ->
    if r < 0
    then raise_errno ~call:"acl_set_tag_type" errno
    else return ()

  let set_qualifier entry qualifier =
    let uuid_t = Osx_membership.uuid in
    Membership.Id.to_uuid qualifier
    >>= fun uuid ->
    let qualifier_p = coerce uuid_t (ptr void) uuid in
    C.set_qualifier entry qualifier_p
    >>= fun (r, errno) ->
    if r < 0
    then raise_errno ~call:"acl_set_qualifier" errno
    else return ()

  let set_entry entry_p entry =
    let entry_struct = !@ entry_p in
    get_permset entry_struct
    >>= fun permset ->
    clear_perms permset
    >>= fun () ->
    Perm.Set.fold (fun perm m ->
      m >>= fun () ->
      add_perm permset perm
    ) entry.Entry.perms (return ())
    >>= fun () ->
    set_tag_type entry_struct entry.Entry.tag
    >>= fun () ->
    set_qualifier entry_struct entry.Entry.qualifier

  let make_acl acl =
    init (List.length acl)
    >>= fun acl_p ->
    List.fold_left (fun m entry ->
      m >>= fun () ->
      create_entry acl_p
      >>= fun entry_p ->
      set_entry entry_p entry
    ) (return ()) acl
    >>= fun () ->
    return acl_p

  let set path acl =
    make_acl acl
    >>= fun acl_p ->
    catch (fun () ->
      set_file path acl_p
      >>= fun () ->
      free (!@ acl_p)
    ) (fun exn ->
      free (!@ acl_p)
      >>= fun () ->
      fail exn
    )

  let fset fd acl =
    make_acl acl
    >>= fun acl_p ->
    catch (fun () ->
      set_fd (int_of_fd fd) acl_p
      >>= fun () ->
      free (!@ acl_p)
    ) (fun exn ->
      free (!@ acl_p)
      >>= fun () ->
      fail exn
    )
end

module C = struct
  type 'a m = 'a
  include Osx_acl_bindings.C(Osx_acl_generated)
end
include Make(Osx_membership.M)(Osx_membership)(C)
