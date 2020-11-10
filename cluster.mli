(** A module that matches [EntryType] is suitable for use as the type of entry
    in a [Cluster]. *)
module type EntryType = sig
  type t
  val create_entry : string list -> t
  val update_field : Field.t -> t -> t
  val to_list : t -> string list
end

(* Schema will probably be the same for all of our files, but should allow us
   to mix things up a bit if we want! *)
(** A [Schema] provides information and functions common to a given
    file architecture. *)
module type Schema = sig

  (** [deserialize line] is the deserialized version of [line]. *)
  val deserialize : string -> string list

  (** [serialize data] is the serialized version of [data]. *)
  val serialize : string list -> string

  (** [search filename criterion] is a [search_result] with entries,
      if any, sharing a substring matching [criterion].
      Requires: [filename] adheres to the specified [Schema]. *)
  val search : string -> string -> string list option

  (** [add filename line] is true if [line] was successfully added to
      [filename], and false otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val add : string -> string -> bool

  (** [delete filename id] is true if the line with corresponding [id] was
      successfully deleted, and false otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val delete : string -> int -> bool

  (** [update filename id change] is true if the line with [id] was updated
      with [change], and false otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val update : string -> int -> (string -> string) -> bool
end

(** A [Cluster] stores data entries in a plaintext file as part of a
    decentralized database. *)
module type Cluster = sig

  (** [Entry] is a module representing the type of
      entry in the cluster and functions on them. *)
  module Entry : EntryType

  (** [Sch] is a module with information and functions on the file structure
      important to database operations. *)
  module Sch : Schema

  (** Raised when nothing was found in a search. *)
  exception NotFound of string

  (** [filename] is the name of the plaintext file where data is stored. *)
  val filename : string

  (** [search criterion] is a list containing all entries that contain the
      search criterion.
      Raises [NotFound criterion] if nothing matches [criterion]. *)
  val search : string -> Entry.t list
  (* Searching in the file should not be case sensitive, and as an added
     challenge, we can turn a blind eye to on incorrect character. This is
     easiest to approach with regex. *)

  (** [delete id] removes the entry with id matching [id]. *)
  val delete : int -> bool

  (** [add data] writes the given data to an entry in the cluster. Data is
      presented as a ordered list of fields corresponding to the entry type. *)
  val add : string list -> bool

  (* Note: THIS DOESN'T WORK FOR NON-NUMERICAL Schema *)
  (** [update id change] edits the entry with id [id] with [change]. *)
  val update : int -> Field.t -> bool
end

(** [MakeCluster] is a functor that makes a [Cluster] out of
    a module representing the data entry type, and a module with information
    on file architecture. *)
module type MakeCluster =
  functor (E : EntryType) ->
  functor (S : Schema) ->
    Cluster with module Entry = E and module Sch = S

module NumIDSchema : Schema
module NoIDSchema : Schema