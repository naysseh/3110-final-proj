(* two options... 
   1. make function that checks if user exists as 1 single function, with 
   another function to verify password
   2. function verifies username and then username and password if the first
   part succeeds. *)


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


(* let create_user =
   print_string "\nPlease enter a username for your new account.";
   let username = read_line () in 
   enter_user username *)
(* if check_user username then 
   print_string "\nPlease enter a password."
   else 
   print_string "\nThis username already exists. Please enter a new username."; *)


(* takes in username and uses backend func to get pass if exists*)
let check_user user =
  User.log_in user 

(* need to add data verification for given input *)
let main () =
  ANSITerminal.(print_string [magenta]
                  "\n\nWelcome to TASKIO\n");
  print_endline "Please enter your username, or the word \"create\" to create a new user.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | username -> ()


let () = main ()