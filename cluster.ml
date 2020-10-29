module type EntryType = sig
  type id
  type t
  val create_entry : string list -> t
  val string_of_entry : t -> string
end

module type Cluster = sig
  module Entry : EntryType

  type id = Entry.id
  type entry = Entry.t

  exception NotFound of string

  val filename : string
  val search : string -> entry list
  val delete : id -> unit
  val add : string list -> unit
  val update : string -> string -> id -> unit
end

module type MakeCluster =
  functor (E : EntryType) -> (Cluster with module Entry = E)