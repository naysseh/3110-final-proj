open Field

exception User_Not_In_Team of string
exception Database_Fatal_Error of string

type user = {tasks : Types.task list; teams : Types.team list; 
             role : user_access}
(**[log_in username] is a string that indicates the password that is expected 
   if the user exists. 
   Raises: NotFound if the user doesnt exist*)
val log_in : string -> string

val create_session : string -> user

(**[manager_task_write username data team tasks] writes data into the file and 
   associates it to the user username. Returns the new task list containing the
   added task to verify that the correct Database function was used. 
   Requires: data is of a correct format corresponding to the team.
   Raises: Database_Fatal_Error code if some database error has occured. code 
   indicates the error.*)
val manager_task_write : string -> string list -> Types.team -> 
  Types.task list -> Types.task list

(**[manager_task_remove id tasks] is the task list with task with ID id removed.
   The corresponding task is also removed from the task file. If no task with ID
   id is present than the task list is unmodified. 
   Raises: Database_Fatal_Error code if some database error has occured. code 
   indicates the error.*)
val manager_task_remove : int -> Types.task list -> Types.task list

(**[manager_task_edit id field value tasks] *)
val manager_task_edit : int -> string -> string 
  -> Types.task list -> Types.task list

(**[get_team_tasks team] is the list of tasks associated with all the members of 
   the team.*)
val get_team_tasks : Types.team -> Types.task list

(**[get_task_by_id tasks id] is the task with id id in tasks
   Raises: Not_Found if no task with id id exists in the list.*)
val get_task_by_id : Types.task list -> int -> Types.task

val get_team : string -> Types.team