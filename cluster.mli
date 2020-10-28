(** A module that matches [EntryType] is suitable for use as the type of entry
    in a [Cluster]. *)
module type EntryType = sig
  type id
  type t
  val create_entry : string list -> t
  val string_of_entry : t -> string
end

(** A [Cluster] stores data entries in a plaintext file as part of a
    decentralized database. *)
module type Cluster = sig

  (** [Entry] is a module representing the type of
      entry in the cluster and functions on them. *)
  module Entry : EntryType

  (** [id] is the type of identifiers of entries in the cluster. *)
  type id = Entry.id

  (** [entry] is the type of entry in the cluster. *)
  type entry = Entry.t

  (** Raised when nothing was found in a search. *)
  exception NotFound of string

  (** [filename] is the name of the plaintext file where data is stored for
      the cluster. *)
  val filename : string

  (** [search criterion] is a list containing all entries that contain the
      search criterion.
      Raises [NotFound] if nothing matches [criterion]. *)
  val search : string -> entry list

  (** [delete id] removes the entry with id matching [id]. *)
  val delete : id -> unit

  (** [add data] writes the given data to an entry in the cluster. Data is
      presented as a ordered list of fields corresponding to the entry type. *)
  val add : string list -> unit

  (** [update change field id] edits the entry with id [id] so its field
      [field] contains [change]. *)
  val update : string -> string -> id -> unit
end

(** A [MakeCluster] is a functor that makes a [Cluster] out of
    a module representing the data entry type. *)
module type MakeCluster =
  functor (E : EntryType) -> Cluster with module Entry = E