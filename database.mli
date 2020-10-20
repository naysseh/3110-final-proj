(*Signature for database.ml*)

type task = {id : int; assignee : string; title : string; 
             status : string; description : string}
type team = {team_name : string; members : string list}
type search_result = 
  | Success of string list
  | Unsuccessful of string

(** Raised when nothing was found in a search*)
exception NotFound of string

(** [search_tasks criterion] is a list containing all the tasks that contain the
    search critetion.
    Raises [NotFound] if nothing was found correspondent to the search 
    condition. *)
val search_tasks : string -> task list

(** [search_teams criterion] is a list containing all the teams that contain the
    search critetion.
    Raises [NotFound] if nothing was found correspondent to the search 
    condition. *)
val search_teams : string -> team list

(** [add_data filename data] is a function that writes the given [data] into 
    the file found at [filename]. *)
val add_data : string -> string -> unit

val add_task_data: string -> unit
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