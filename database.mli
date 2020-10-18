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
val get_data : string -> string -> bool -> 'a

(** [add_data filename data] is a function that writes the given [data] into 
    the file found at [filename]. *)
val add_data : string -> 'a -> unit