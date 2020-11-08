module type EntryType = sig
  type t
  val create_entry : string list -> t
  val update_field : Field.t -> t -> t
  val to_list : t -> string list
end

module type Schema = sig
  val deserialize : string -> string list
  val serialize : string list -> string
  val search : string -> string -> string list option
  val add : string -> string -> bool
  val delete : string -> int -> bool
  val update : string -> int -> (string -> string) -> bool
end

module type Cluster = sig
  module Entry : EntryType
  module Sch : Schema

  exception NotFound of string

  val filename : string
  val search : string -> Entry.t list
  val delete : int -> bool
  val add : string list -> bool
  val update : int -> Field.t -> bool
end

module type MakeCluster =
  functor (E : EntryType) ->
  functor (S : Schema) ->
    Cluster with module Entry = E and module Sch = S