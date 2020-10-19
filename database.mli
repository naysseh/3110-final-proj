(*Signature for database.ml*)

(** Raised when a non-existing file is accessed. *)
exception InvalidFile of string

(** Raised when nothing was found in a search*)
exception Not_found of string

(**[get_data filename condition in_description] is a record representing the 
   particular data type, supported by the database
   Raises [InvalidFile filename] if such file does not exist
   Raises [NotFound condition] if nothing was found correspondent to the search 
   condition. *)
val get_task_data : string -> string -> 'a

(** [add_data filename data] is a function that writes the given [data] into 
    the file found at [filename]. *)
val add_data : string -> 'a -> unit

(** [update_data filename predicate field data] writes over property [field] of 
    the data identified by [predicate] in file [filename] with [data].

    Requires: [predicate : 't -> bool] and [field] are valid for type ['t]
    represented by the data stored in [filename]. As such, there must also be
    at least one entry in [filename] that satisfies [predicate]. *)
val update_data : string -> ('t -> bool) -> string -> string -> unit
(* Rather than giving a predicate that I am going to require returns true for at least
   one data entry, I would rather have a separate [search] function give the 
   locations at which to make changes.
   Note: what to do about data that is longer than existing data; do the IO
   functions write over, or insert? *)