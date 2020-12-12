(*Signature for user.mli*)
type user_access = 
  | Manager
  | Engineer
  | Scrummer

exception User_Not_In_Team of string

type user = {tasks : Types.task list; teams : Types.team list; 
             role : user_access}
(**[log_in username] is a string that indicates the password that is expected 
   if the user exists. 
   Raises: NotFound if the user doesnt exist*)
val log_in : string -> string

val create_session : string -> user

val manager_task_write : string -> string list -> Database.team -> unit

val get_team : string -> Types.team