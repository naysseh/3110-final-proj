(*Signature for user.mli*)
type user_access = 
  | Manager
  | Engineer
  | Scrummer

type user = {tasks : Database.task list; teams : Database.team list; 
             role : user_access}
(**[log in username] is a bool string pair where the bool indicates whether 
   the username is valid and the string indicates the password that is expected 
   if the bool is true*)
val log_in : string -> string

val create_session : string -> user