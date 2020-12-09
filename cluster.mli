(** A module that matches [EntryType] is suitable for use as the type of entry
    in a [Cluster]. *)
module type EntryType = sig
  type t
  val assoc_file : string
  val create_entry : string list -> t
  val update_field : Field.t -> t -> t
  val to_string_list : t -> string list
  val to_field_list : t -> Field.t list
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

  (** [rep_ok ?aux filename] is whether the file corresponding to [filename]
      adheres to the specified Schema. Since Schemas are used in conjunction
      with other modules, an [aux] function may be provided to check further
      line-specific conditions. *)
  val rep_ok : ?aux:(string -> string) -> string -> bool

  (** [search filename criteria] is the result of a search on [filename] with 
      entries, if any, satisfying the [criteria] function.
      Requires: [filename] adheres to the specified [Schema]. *)
  val search : string -> (string -> bool) -> string list option

  (** [add filename line] is true if [line] was successfully added to
      [filename], and false otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val add : string -> string -> bool

  (** [delete filename selection] is [Ok List.length selection] if the operation
      is successful, and [Error e] otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val delete : string -> string list -> (int, string) result

  (** [update filename change] is true if line(s) were altered according to 
      [change], and false otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val update : string -> (string -> string) -> bool
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

  (** [filename] is the name of the plaintext file where data is stored. *)
  val filename : string ref

  (** [bind teamname] focuses the cluster on the file for [teamname]. *)
  val bind : string -> unit

  (** [unbind ()] unbinds the cluster from the current team-specific file. *)
  val unbind : unit -> unit

  (* [rep_ok ()] is whether the current bound file is a valid Cluster according
     to the Sch and Entry implementation. *)
  val rep_ok : unit -> bool

  (** [search criterion] is a list containing all entries that match the
      criterion, if any.
      Raises [Not_found] if nothing matches any [criteria]. *)
  val search : (Field.t -> bool) -> Entry.t list
  (* Searching in the file should not be case sensitive, and as an added
     challenge, we can turn a blind eye to on incorrect character. This is
     easiest to approach with regex. *)

  (** [delete criterion] is [Ok x] if [x] entries meeting [criterion] were
      deleted, and [Error e] if an exception described in [e] was raised
      in the process, leaving the cluster untouched. *)
  val delete : (Field.t -> bool) -> (int, string) result

  (** [add data] writes the given data to an entry in the cluster. Data is
      presented as a ordered list of fields corresponding to the entry type. *)
  val add : string list -> bool

  (* Note: THIS DOESN'T WORK FOR NON-NUMERICAL Schema *)
  (** [update criterion change] edits the entry(ies) matching [criterion] with
      [change]. *)
  val update : (Field.t -> bool) -> Field.t -> bool
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