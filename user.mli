(*Signature for user.mli*)

(**[log in username] is a bool string pair where the bool indicates whether 
   the username is valid and the string indicates the password that is expected 
   if the bool is true*)
val log_in : string -> bool * string