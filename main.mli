(** The main entry point for the Trakio interface. *)

type input_type = 
  | Password
  | Username

(** [validate_input input i_type] validates a given string based on its 
    input_type (username or password). 
    Restrictions include: username must be between 4 and 20 chars, password no 
    smaller than 8 chars. Usernames cannot contain special characters, 
    but passwords can (except backslash). *)
val validate_input : string -> input_type -> bool