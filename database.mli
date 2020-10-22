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

(** [delete_task filename id] removes the task with id matching [id] from the 
    file corresponding to [filename].
    Requires: [filename] is the store for tasks, and a task of id [id]
    exists in the file. *)
val delete_task : string -> int -> unit

(** [edit_task_data change field id] edits the textual representation of the 
    task data with id number id, changing the field field to change *)
val edit_task_data : string -> string -> int -> unit

(** [add_data filename data] is a function that writes the given [data] into 
    the file found at [filename]. *)
val add_data : string -> unit