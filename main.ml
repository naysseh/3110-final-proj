(* NOTES: to do
   - figure out a way to include functions in the validate_print, so that 
     it can either succeed/re-call the function ... or just create pre-validation
     every time user/pass verification is needed when inputting a string ... *)

type input_type = 
  | Password
  | Username 

(* create session is gonna return a user - look in user.ml file *)

(* if true, input is permitted - false, need to enter new input *)
(* input is the given data, i_type is the type of data - user,pass,etc. *)
(* username must be between 4 and 20 chars, password no smaller than 8 chars. 
   Usernames cannot contain special characters, but passwords can 
   (except backslash). *)

let validate_input input i_type = 
  let new_input = String.trim input in 
  let length = String.length new_input in 
  if i_type = Username && (length < 4 || length > 20) then false 
  else if i_type = Password && length < 8 then false 
  else if String.contains new_input ' ' then false 
  else if i_type = Username && 
          (Str.string_match (Str.regexp "[^a-zA-Z0-9]*") 
             new_input 0) = true then false 
  else if i_type = Password && (String.contains new_input '\\') = true 
  then false 
  else true 

(* regexp to exclude special chars: "^[\\[\\$\\^\\.\\*\\+\\?]*" *)

(* validate_print takes in an input ([validation]) and then checks it as a 
   valid input. if false, it matches it with its type (user or password).
   It returns a record containing t/f and a message.  *)
let validate_print validation i_type = 
  let result = validate_input validation i_type in 
  match result with
  | false -> if i_type = Username then 
      (false, "Your username is invalid. Please be sure you adhere to the following: 
  \nNo spaces or special characters, and be sure the length is between 4 and 20 characters.") 
    else (false, "Your password is invalid. Please be sure you adhere to the following: 
  \nNo spaces, no backslashes, and be sure that the length is greater than 8 characters.")
  | true -> (true, "success")

(* if a user enters a username that already exists, direct them to enter a new one. 
   w non-existing username, create new session w create_session *)
let new_pass user = 
  print_endline "Please enter a password for your new account";
  match read_line () with 
  | exception End_of_file -> failwith "oops"
  | pass -> "use create_session here"

let rec new_user x =
  print_endline "Please enter a username for your new account, no spaces or 
  special characters.";
  match read_line () with 
  | exception End_of_file -> failwith "lol"
  | user -> 
    match User.log_in user with 
    | exception Database.NotFound user -> new_pass user
    | string -> 
      print_endline "user already taken -- need to implement"; 
      new_user string

(* takes in username, returns password if user exists, otherwise error msg *)
let check_user user =
  try User.log_in user with  
    Database.NotFound user -> "Your username does not exist. Please enter again
    or create a new user."

let rec password_verify pass =
  print_endline "Please enter your password.\n";
  print_string  "> ";
  match read_line () with 
  | exception End_of_file -> failwith "uhh"
  | input_pass -> 
    if input_pass = pass then print_endline "success" 
    else begin print_endline 
        "Your password does not match your inputted username. Please try again.\n";
      password_verify pass
    end

(* use this to get a list of tasks... print in table using pretty printing 
   and have columns based on name,type,finished?,description etc.? *)
(* put all functions inside this one - call check_user user |> pass_verify *)
(* use this as the "run" function basically in main () *)
let get_tasks user : unit = 
  failwith "not done"


let pp_cell fmt cell = Format.fprintf fmt "%s" cell

(* create array matrix with tasks, make a row with titles
   id, assignee, title, descr, status *)
let create_table = 
  failwith "to do"

(* need to add data verification for given input *)
(* can't split up these strings ... but they go over the char limit :( *)
let main () =
  ANSITerminal.(print_string [magenta] 
                  "──────────────────────────────┬─────────────────────────────────────────────────────────────┬───────────────────────────────");
  ANSITerminal.(print_string [magenta]
                  "\n                              |");
  ANSITerminal.(print_string [green] "                    Welcome to TASKIO");
  ANSITerminal.(print_string [magenta] "                        |\n");
  ANSITerminal.(print_string [magenta]
                  "                              └─────────────────────────────────────────────────────────────┘\n" );
  print_endline "Please enter your username, or the word \"create\" to create a new user.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "create" -> () (*new_user "create"*)
  | username -> (*get_tasks*) (check_user username |> password_verify)


let () = main ()