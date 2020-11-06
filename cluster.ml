module type EntryType = sig
  type id
  type t
  type field
  val create_entry : string list -> t
  val update_field : field -> t -> t
  val string_of_entry : t -> string list
end

module type Schema = sig
  type id
  val deserialize : string -> string list
  val serialize : string list -> string
  val search : string -> string -> string list option
  val add : string -> string -> bool
  val delete : string -> id -> bool
  val update : string -> id -> (string -> string) -> bool
end

module type Cluster = sig
  module Entry : EntryType
  module Sch : Schema

  type id = Entry.id
  type field = Entry.field
  type entry = Entry.t

  exception NotFound of string

  val filename : string
  val search : string -> entry list
  val delete : id -> bool
  val add : string list -> bool
  val update : id -> field -> bool
end

module type MakeCluster =
  functor (E : EntryType) ->
  functor (S : Schema with type id = E.id) ->
    Cluster with module Entry = E and module Sch = S