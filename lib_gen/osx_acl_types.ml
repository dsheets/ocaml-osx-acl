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

module C(F: Cstubs.Types.TYPE) = struct

  let t = F.(typedef (ptr void) "acl_t")
  let entry = F.(typedef (ptr void) "acl_entry_t")
  let permset = F.(typedef (ptr void) "acl_permset_t")

  module Entry_id = struct
    let t = F.(typedef int "acl_entry_id_t")

    let first = F.constant "ACL_FIRST_ENTRY" t
    let next = F.constant "ACL_NEXT_ENTRY" t
    let last = F.constant "ACL_LAST_ENTRY" t
  end

  module Perm = struct
    let t = F.(typedef int "acl_perm_t")

    let read_data = F.constant "ACL_READ_DATA" t
    let write_data = F.constant "ACL_WRITE_DATA" t
    let execute = F.constant "ACL_EXECUTE" t
    let delete = F.constant "ACL_DELETE" t
    let append_data = F.constant "ACL_APPEND_DATA" t
    let delete_child = F.constant "ACL_DELETE_CHILD" t
    let read_attributes = F.constant "ACL_READ_ATTRIBUTES" t
    let write_attributes = F.constant "ACL_WRITE_ATTRIBUTES" t
    let read_extattributes = F.constant "ACL_READ_EXTATTRIBUTES" t
    let write_extattributes = F.constant "ACL_WRITE_EXTATTRIBUTES" t
    let read_security = F.constant "ACL_READ_SECURITY" t
    let write_security = F.constant "ACL_WRITE_SECURITY" t
    let change_owner = F.constant "ACL_CHANGE_OWNER" t
    let synchronize = F.constant "ACL_SYNCHRONIZE" t
  end

  module Tag = struct
    let t = F.(typedef int "acl_tag_t")

    let undefined = F.constant "ACL_UNDEFINED_TAG" t
    let allow = F.constant "ACL_EXTENDED_ALLOW" t
    let deny = F.constant "ACL_EXTENDED_DENY" t
  end

  let type_ = F.(typedef int "acl_type_t")
  let type_extended = F.constant "ACL_TYPE_EXTENDED" type_

end
