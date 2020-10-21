(*Signature for database.ml*)

type task = {id : int; assignee : string; title : string; 
             status : string; description : string}
type team = {team_name : string; members : string list}

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

(** [update_task_data filename id field data] writes over property [field] of 
    the task identified by [id] in file [filename] with [data]. *)
val update_task_data : string -> int -> string -> string -> unit