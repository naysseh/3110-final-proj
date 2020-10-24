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

(** [delete_task id] removes the task with id matching [id]. *)
val delete_task : int -> unit

(** [edit_task change field id] edits the textual representation of the 
    task data with id number [id], changing the field [field] to [change]. *)
val edit_task : string -> string -> int -> unit

(** [add_task data] writes the given [data] into a task. The new task is added
    to the top of the task database. *)
val add_task : string list -> unit

val add_data : string -> string list -> unit