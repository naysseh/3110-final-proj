open Field

exception User_Not_In_Team of string

type user = {tasks : Types.task list; teams : Types.team list; 
             role : user_access}
(**[log_in username] is a string that indicates the password that is expected 
   if the user exists. 
   Raises: NotFound if the user doesnt exist*)
val log_in : string -> string

val create_session : string -> user

(**[manager_task_write username data team] writes data into the file and 
   associates it to the user username. Returns bool to verify that the correct
   Database function was used. If true everything is correct, otherwise 
   something went  wrong. 
   Requires: data is of a correct format corresponding to the team.
   Raises: Database_Fatal_Error code if some database error has occured. code 
   indicates the error.*)
val manager_task_write : string -> string list (*-> Database.team*) -> bool

val get_team : string -> Types.team