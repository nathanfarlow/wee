type t

include Jump_table_intf.S with type t := t

val create : unit -> t