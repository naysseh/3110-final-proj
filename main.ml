(* let rec enter_user user = 
   if check_user user then 
    begin
      print_string "please enter a different name. that username already exists";
      let new_user = read_line () in 
      enter_user new_user 
    end
   else 
    begin 
      print_string "please enter a password for your new username.";
      let new_pass = read_line () in
      () (* need to put a function here to create a user *)
    end  *)

(* if true, input is permitted - false, need to enter new input *)
let validate_input input = 
  failwith "not done"


(* use this to get a list of tasks... print in table using pretty printing 
   and have columns based on name,type,finished?,description etc.? *)
let get_tasks user = 
  failwith "not done"


(* if a user enters a username that already exists, direct them to enter a new one. 
   w non-existing username, create new session w create_session *)
let new_pass user = 
  print_endline "Please enter a password for your new account";
  match read_line () with 
  | exception End_of_file -> failwith "oops"
  | pass -> "use create_session here"

let rec new_user x =
  print_endline "Please enter a username for your new account.";
  match read_line () with 
  | exception End_of_file -> failwith "lol"
  | user -> 
    match User.log_in user with 
    | exception Database.NotFound user -> new_pass user
    | string -> 
      print_endline "user already taken -- need to implement"; 
      new_user string

(* try User.log_in user with Database.NotFound user -> () *)
(* if check_user username then 
   print_string "\nPlease enter a password."
   else 
   print_string "\nThis username already exists. Please enter a new username."; *)


(* takes in username, returns password if user exists, otherwise error msg*)
let check_user user =
  try User.log_in user with  
    Database.NotFound user -> "Your username does not exist. Please enter again
    or create a new user." 

let password_verify pass =
  print_endline "Please enter your password.\n";
  print_string  "> ";
  match read_line () with 
  | exception End_of_file -> failwith "uhh"
  | input_pass -> 
    if input_pass = pass then print_endline "success" 
    else print_endline 
        "Your password does not match your inputted username. Please try again." 

(* need to add data verification for given input *)
let main () =
  ANSITerminal.(print_string [magenta]
                  "\n\nWelcome to TASKIO\n");
  print_endline "Please enter your username, or the word \"create\" to create a new user.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "create" -> () (* new_user "create" *)
  | username -> check_user username |> password_verify


let () = main ()