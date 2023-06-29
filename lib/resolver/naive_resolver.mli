type t

include Resolver_intf.S with type t := t

val create : unit -> t