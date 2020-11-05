(** A module that matches [EntryType] is suitable for use as the type of entry
    in a [Cluster]. *)
module type EntryType = sig
  type id
  (* [field] will be a variant with all the fields, likely starting with Id of
     [id] or something, and [t] will structure them in a variant. *)
  type field
  type t
  val create_entry : string list -> t
  (* Creating an entry is really only used to de-serialize data,
     either to return in a search or to edit. But when de-serializing data,
     we leave this to the cluster to check the string. Why? I don't know.
     either we do it here or there. Doing it here is not really in the spirit
     of this module type though. However, we will need some help from this
     module for validating input, so think about how to do that. *)
  val string_of_entry : t -> string list
  (* This is a highly useful operation, but it might depend on
      Cluster-specific logic, like unique field and line delimiters. We can save
      that for the Cluster implementation, which would simply involve
      combining a string list by a certain character and capping with newline.
      Regardless, we should save newlines for Cluster; the field delimiter
      is a valid tossup. *)
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

  (** [modify filename start_id mod_line] is [true] if the file was successfully
      modified according to [mod_file], and false otherwise. *)
  val modify : string -> int -> (string -> int -> out_channel -> unit) -> bool
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

  (** [id] is the type of identifiers of entries in the cluster. *)
  type id = Entry.id

  (** [field] is the type of entry field. *)
  type field = Entry.field

  (** [entry] is the type of entry in the cluster. *)
  type entry = Entry.t

  (** Raised when nothing was found in a search. *)
  exception NotFound of string

  (** [filename] is the name of the plaintext file where data is stored. *)
  val filename : string

  (** [search criterion] is a list containing all entries that contain the
      search criterion.
      Raises [NotFound criterion] if nothing matches [criterion]. *)
  val search : string -> entry list
  (* Searching in the file should not be case sensitive, and as an added
     challenge, we can turn a blind eye to on incorrect character. This is
     easiest to approach with regex. *)

  (* The next three functions need to return something other than unit. First of
     all, the file operations from Pervasives will raise Sys_error if they fuck
     up, so gotta be prepared for that. But this is the only possible error I can
     think of, unless somehow we're successful in adding the data but the
     representation invariant is broken? Then we would need an informative return
     type. But if we're just indicating success or failure, a boolean is fine for
     saying, "We altered the database" or "Nothing was changed due to something
     we're unsure about." *)

  (** [delete id] removes the entry with id matching [id]. *)
  val delete : id -> bool


  (** [add data] writes the given data to an entry in the cluster. Data is
      presented as a ordered list of fields corresponding to the entry type. *)
  val add : string list -> bool

  (** [update id change] edits the entry with id [id] with [change]. *)
  val update : id -> field -> bool
end

(** A [MakeCluster] is a functor that makes a [Cluster] out of
    a module representing the data entry type, and a module with information
    on file architecture. *)
module type MakeCluster =
  functor (E : EntryType) ->
  functor (S : Schema) ->
    Cluster with module Entry = E and module Sch = S