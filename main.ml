type input_type = 
  | Password
  | Username

type task_output = 
  | View
  | Print_All

(** [validate_input input i_type] validates a given [input] based on its 
    [i_type] which is either a username or password. 
    Restrictions include: username must be between 4 and 20 chars, password no 
    smaller than 8 chars. Usernames cannot contain special characters, 
    but passwords can (except backslash). *)
let validate_input input i_type = 
  let new_input = String.trim input in 
  let length = String.length new_input in 
  if i_type = Username && (length < 4 || length > 20) then false 
  else if i_type = Password && length < 8 then false 
  else if String.contains new_input ' ' then false 
  else if i_type = Username && 
          (Str.string_match (Str.regexp "^[a-zA-Z0-9]+$") 
             new_input 0) = false then false 
  else if i_type = Password && (String.contains new_input '\\') = true 
  then false 
  else true 
(* another regexp to exclude certain special chars: 
   "^[\\[\\$\\^\\.\\*\\+\\?]+$" *)

(** [validate_print validation i_type] takes in an input [validation] and then 
    checks it as a valid input. If false, it matches it with its [i_type] 
    (user or password). It returns a bool t/f and prints a message. *)
let validate_print validation i_type = 
  let result = validate_input validation i_type in 
  match result with
  | false -> if i_type = Username then begin 
      print_endline "Your username is invalid. Please be sure you adhere to the following:";
      print_endline 
        "No spaces or special characters, and be sure the length is between 4 and 20 characters.";
      false end 
    else begin 
      print_endline "Your password is invalid. Please be sure you adhere to the following:"; 
      print_endline 
        "No spaces, no backslashes, and be sure that the length is greater than 8 characters."; 
      false end
  | true -> true

(** [new_pass user] takes in password for new account with name [user].
    Verifies their password using helper. If a user enters a username that 
    already exists, direct them to enter a new one w non-existing username. 
    Create new user. *)
let rec new_pass user = 
  print_endline "Please enter a password for your new account \n";
  print_string  "> ";
  let input = read_line () in 
  let validation = validate_print input Password in 
  if validation = false then new_pass user else 
    match input with 
    | exception End_of_file -> failwith "oops"
    | pass -> print_endline "create new user not implemented" 
(* print message to tell user to log in again using their new login *)

(** [new_user username] validates an inputted username and presents the user
    with the options on their input. *)
let rec new_user username =
  print_endline "Please enter a username for your new account, no spaces or 
  special characters. \n";
  print_string  "> ";
  let input = read_line () in 
  let validation = validate_print input Username in 
  if validation = false then new_user "restart" else 
    match input with 
    | exception End_of_file -> Stdlib.exit 0
    | user ->
      match User.log_in user with
      | exception Database.NotFound user -> new_pass user
      | string -> 
        print_endline "user already taken -- restart"; 
        new_user "not done"

(** [check_user user] akes in username, returns password if user exists. *)
let check_user user =
  try User.log_in user with  
    Database.NotFound user -> "Your username does not exist. Please enter again
    or create a new user."

(** [password_verify user pass] takes in a [user] and [pass] and verifies that 
    the inputted password matches the username in the login base. Prompts the 
    user to re-enter if the password does not match the username. *)
let rec password_verify user pass =
  print_endline "Please enter your password, or enter 0 to quit. \n";
  print_string  "> ";
  match read_line () with 
  | exception End_of_file -> failwith "failed"
  | input_pass -> 
    if input_pass = pass then 
      begin 
        print_string ("\n");
        ANSITerminal.(print_string [green] "TASKS: ");
        print_string ("\n"); 
        try User.create_session user with Database.NotFound user -> begin
            print_endline "User not in database/empty user.";
            {User.tasks=[]; User.teams=[]; User.role=Field.Engineer} end
      end 
    else if input_pass = "0" then Stdlib.exit 0
    else begin print_endline 
        "Your password does not match your inputted username. Please try again.\n";
      password_verify user pass
    end

(** [tasks_rec tasks type_T] prints out a formatted view of tasks. If type_t is
    View, then it includes the assignee name.  *)
let rec tasks_print_rec (tasks : Types.task list) (type_t : task_output) = 
  match tasks with 
  | [] -> ()
  | h :: t -> begin 
      if type_t = View then 
        print_string (h.assignee ^ " - " )
      else print_string "";
      print_endline (h.title ^ ": " ^ h.status ^ " --> " ^
                     h.description ^ " (id: " ^ string_of_int h.id ^")");
      tasks_print_rec t type_t
    end 

(** [team_lists_string team_l] takes in a list of teams and prints them 
    separated by commas. *)
let rec team_lists_string (team_l : Types.team list) = 
  match team_l with 
  | [] -> ""
  | h :: t -> String.concat ", " 
                (Types.Team.to_string_list h) ^  "\n" ^ (team_lists_string t)

(** [team_select user] is a helper for [add_tasks_input] to display the teams
    that a manager is a part of in order for the manager to determine which 
    team they will be editing tasks for. *)
let rec team_select (user : User.user) = 
  print_endline 
    "Please enter the name of the team from the list below that you would like to edit.";
  print_endline "The name is the first element of the list shown.\n";
  ANSITerminal.(print_string [green] "TEAMS: ");
  print_endline (team_lists_string user.teams);
  print_string "> ";
  (* let team_name = User.get_team (read_line ()) in *)
  try User.get_team (read_line ()) with Database.NotFound team_name -> (
      print_endline 
        "Team name entered does not exist. Please enter a valid teamname.\n";
      team_select user)

(** *)

(** [print_input user] is a helper for add_tasks_input that simply asks 
    the user for input and prints out a string representation of the users 
    desired input on tasks. Returns the team, assignee, title, status, 
    and description inputted by the user. *)
let print_input user = 
  let team = team_select user in
  print_endline 
    "Please enter the name of the user you would like to add a task to:\n";
  print_string  "> ";
  let assignee = read_line () in 
  print_endline "Please enter the title of the task:\n";
  print_string  "> ";
  let title = read_line () in 
  print_endline "Please enter the status of the task:\n";
  print_string  "> ";
  let status = read_line () in 
  print_endline "Please enter the description of the task:\n";
  print_string  "> ";
  let description = read_line () in 
  print_endline "Please confirm that this is the task you would like to add. 
   Enter 1 to confirm, or 0 to re-enter. \n";
  print_endline ("Team name: " ^ team.teamname ^ "\n");
  print_endline ("Assignee: " ^ assignee ^ "\n" ^ "Title: " ^ title ^ "\n" ^
                 "Status: " ^ status ^ "\n" ^
                 "Description: " ^ description ^ "\n");
  print_string "> ";
  (team, assignee, title, status, description)

(** [add_tasks_input user] is the function that takes in input from the user
    on the task they would like to add.  *)
let rec add_tasks_input user = 
  let (team, assignee, title, status, description) = print_input user in 
  let rec entry input = 
    match read_line () with 
    | "1" -> ( 
        match 
          User.manager_task_write assignee [title; status; description] 
            team user.tasks with 
        | t_list -> (print_endline "Success."; Stdlib.exit 0)
        | exception User.User_Not_In_Team assignee -> begin 
            print_endline 
              "This user was not in the team listed. Please reenter.";
            add_tasks_input user end )
    | "0" -> add_tasks_input user
    | _ -> (print_endline "Not a valid input. Please enter either 1 or 0.";
            entry user) 
  in entry user

(** [manager_add_option user] takes in a user that has the role of a manager 
    and displays their options under the action "add." *) 
let rec manager_add_option user = 
  print_endline "Please enter what you would like to add:";
  print_endline "Task | Team \n";
  match String.lowercase_ascii (read_line ()) with 
  | "task" -> add_tasks_input user 
  | "team" -> ()
  | _ -> (print_endline 
            "Invalid input. Please enter either \"Task\" or \"Team\" ";
          manager_add_option user)

(** [format_task task] formats the task into a readable format with obvious
    fields. *)
let format_task (task : Types.task) = 
  print_endline 
    ("Assignee: " ^ task.assignee ^ "\nTitle: " ^ task.title ^
     "\nStatus: " ^ task.status ^ "\nDescription: " ^ task.description); ()

(** [edit_field id tasks] takes in an id number and list of tasks and 
    asks the user which field they would like to input from the task 
    specified. *)
let rec edit_field id tasks = 
  print_endline "Which field would you like to edit? Enter from:";
  print_endline "Assignee | Title | Status | Description\n";
  let task = User.get_task_by_id tasks id in 
  format_task task;
  print_string "\n> ";
  let rec entry id = 
    match String.lowercase_ascii (read_line ()) with 
    | "assignee" -> "assignee"
    | "title" -> "title"
    | "status" -> "status"
    | "description" -> "description"
    | _ -> (
        print_endline "Invalid input. Please enter either:";
        print_endline "Assignee | Title | Status | Description\n";
        entry id)
  in entry id

(** [field_entry user] validates the id entry for the user, making sure it 
    is an int. *)
let rec id_entry user = 
  let id = read_line () in 
  if Str.string_match (Str.regexp "^[0-9]+$") id 0 
  then int_of_string id else  
    (print_endline "Please enter a valid id number. (Int input only)";
     print_string "> ";
     id_entry user)

(** [manager_edit user] takes in a user with role manager and asks for input 
    on where they would like to edit a task.  *)
let rec manager_edit user = 
  let team = team_select user in
  print_endline 
    "Please select the id number of the task you would like to edit.\n";
  let tasks_list = User.get_team_tasks team in
  tasks_print_rec (tasks_list) View;
  print_string "> ";
  let id = id_entry user in  
  let field = edit_field id tasks_list in 
  print_endline ("What would you like " ^ field ^ " to be updated to?");
  let value = read_line () in 
  match User.manager_task_edit id field value tasks_list with 
  | t_list -> (print_endline "Success."; Stdlib.exit 0)
  | exception User.Database_Fatal_Error "Database error" -> 
    (print_endline "A problem occured in the database. Please retry";
     manager_edit user)

(** [manager_actions user] takes in a User.user that has the role of manager 
    and displays them the possible actions they can take. *)
let rec manager_actions user = 
  print_endline 
    "What action would you like to do? Please enter one of the following:";
  print_endline "Add | Delete | Edit | Quit \n";
  match String.lowercase_ascii (read_line ()) with 
  | "add" -> manager_add_option user
  | "delete" -> () 
  | "edit" -> manager_edit user
  | "quit" -> Stdlib.exit 0
  | _ -> 
    (print_endline 
       "Invalid input. 
       Please enter either \"Add\", \"Delete\", \"Edit\", or \"Quit\"\n"; 
     manager_actions user)

(** Offer a user the actions that come with their role. If the user is a 
    Engineer or Scrummer, they do not have access to alter tasks, only to
    view them. *)
let rec actions (user : User.user) = 
  let role = user.role in
  match role with 
  | Manager -> manager_actions user
  | Engineer -> ()
  | Scrummer -> ()

(** [get_tasks user] takes in a string [user] and attempts to login. If 
    successful, will print a user's list of tasks, and direct them to their 
    actions. *)
let get_tasks user = 
  let user_type = check_user user |> password_verify user in 
  tasks_print_rec user_type.tasks Print_All;
  print_newline () ;
  actions user_type 

let main () =
  ANSITerminal.(print_string [magenta] 

                  "──────────────────────────────┬─────────────────────────────────────────────────────────────┬─────────────────────────────");
  ANSITerminal.(print_string [magenta]
                  "\n                              |");
  ANSITerminal.(print_string [yellow] "                    Welcome to ");
  ANSITerminal.(print_string [green] "TRAKIO");
  ANSITerminal.(print_string [magenta] "                        |\n");
  ANSITerminal.(print_string [magenta]
                  "                              └─────────────────────────────────────────────────────────────┘\n" );
  print_endline "Please enter your username, or the word \"create\" to create a new user.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "create" -> new_user "create"
  | username -> get_tasks username

let () = main ()