(* alter these types, use as results for check_user. failed should return
   why it failed - either that username doesn't exist or that password doesn't
   match the given username (given that the username exists) *)
type user_search_result = 
  | Success 
  | Failed 

(* two options... 
   1. make function that checks if user exists as 1 single function, with 
   another function to verify password
   2. function verifies username and then username and password if the first
   part succeeds. *)


(* need to take in a username input from the user, use a function
   to coordinate w backend and check that the username exists, if succeeded, 
   return ... success or true? if fails return error stating error  *)
let check_user user =
  print_string "hi";
  true

let rec enter_user user = 
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
    end 


(* let create_user =
   print_string "\nPlease enter a username for your new account.";
   let username = read_line () in 
   enter_user username *)
(* if check_user username then 
   print_string "\nPlease enter a password."
   else 
   print_string "\nThis username already exists. Please enter a new username."; *)


let main () =
  ANSITerminal.(print_string [magenta]
                  "\n\nWelcome to TASKIO\n");
  print_endline "Please enter your username, or the word \"create\" to create a new user.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | string -> ()

let () = main ()