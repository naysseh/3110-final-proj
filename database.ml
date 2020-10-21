(*Implementation of the data file management*)

(********Types********)
type task = {id : int; assignee : string; title : string; 
             status : string; description : string}
type team = {team_name : string; members : string list}
type search_result = 
  | Success of string list
  | Unsuccessful of string

(********Types********)

(********Exceptions********)
exception NotFound of string
(********Exceptions********)

(********Constructors********)
let create_task string = 
  match String.split_on_char ';' string with
  | id::assignee::title::status::description -> 
    {id = int_of_string id; assignee = assignee; title = title; 
     status = status; description = List.hd description}
  | _ -> failwith "mistake with the reading" 

let create_team string = 
  match String.split_on_char ';' string with
  | h::t -> {team_name = h; members = t}
  | _ -> failwith "mistake creating a team"
(********Constructors********)

(********General Helpers********)
let string_contains str1 str2= 
  let len1 = String.length str1 and len2 = String.length str2 in
  if len1 < len2 then false else
    let rec check i = 
      if i > len1 - len2 then false else
      if (str2 = String.sub str1 i len2) then true else check (succ i) in
    check 0

(** [string_of_task task] is the string representation of [task]. *)
let string_of_task (task : task) : string = 
  string_of_int task.id ^ ";" ^ task.assignee ^ ";" ^ task.title ^ ";" ^ task.status ^ ";"
  ^ "\"" ^ task.description ^"\""

let get_search_results filename criterion : search_result = 
  let channel = open_in filename in
  let rec parse_line chnl acc= 
    match input_line chnl with 
    | x -> if string_contains x criterion then parse_line chnl (x :: acc) 
      else parse_line chnl acc
    | exception End_of_file -> acc in
  let results = parse_line channel [] in
  close_in channel;
  if List.length results != 0 then Success results 
  else Unsuccessful ("Could not find anything matching: " ^ criterion)

let rec form_list lst acc constr = 
  match lst with 
  | [] -> acc
  | h::t ->  form_list t (constr h :: acc) constr

let form_task_list task_strings = 
  form_list task_strings [] create_task

let form_teams_list team_strings = 
  form_list team_strings [] create_team

(* We got lucky since the only field we need to change are strings. If we need
   to alter the id of a task, then we need to create a new algebraic data type. *)
let update_task_field task data = function
  | "id" -> failwith "Id is not mutable"
  | "assignee" -> {task with assignee=data}
  | "title" -> {task with title=data}
  | "status" -> {task with status=data}
  | "description" -> {task with description=data}
  | _ -> raise Not_found

(********General Helpers********)

let search_tasks criterion = 
  match get_search_results "issues.txt" criterion with
  | Success x -> form_task_list x
  | Unsuccessful x -> raise (NotFound criterion)

let search_teams criterion =
  match get_search_results "teams.txt" criterion with
  | Success x -> form_teams_list x
  | Unsuccessful x -> raise (NotFound criterion)

let add_data filename data = 
  let channel = open_out_gen [Open_append] 0o640 filename in
  output_string channel ("\n" ^ data); 
  close_out channel

(* testing out input from command line here and seeing how it works :) should
   id autofill to the next available ID? *)
let add_task_data filename = 
  let channel = open_out_gen [Open_append] 0o640 filename in
  print_string "enter id ";
  let new_id = read_int () in
  print_string "enter assignee ";
  let new_assignee = read_line () in
  print_string "enter title ";
  let new_title = read_line () in 
  print_string "enter status ";
  let new_status = read_line () in 
  print_string "enter description ";
  let new_descr = read_line () in
  output_string channel ("\n" ^ string_of_task(
      {id = new_id; assignee = new_assignee; title = new_title; 
       status =  new_status; description = new_descr})); 
  close_out channel

let get_task_and_pos_by_id filename id =
  let channel = open_in filename in
  let rec parse_line ic id depth =
    let line = input_line ic in
    match create_task line with 
    (* Think about a higher-order function that parametrizes comparison
       and what is returned. *)
    | x when x.id = id -> (pos_in ic - String.length line - 1, create_task line)
    | x -> parse_line ic id (depth + String.length line) in
  let pos = parse_line channel id 0 in
  close_in channel; pos

let update_task_data
    (filename : string)
    (id : int)
    (field : string)
    (data : string) : unit =
  let (pos, task) = get_task_and_pos_by_id filename id in
  let oc = open_out_gen [Open_wronly] 0o640 filename in
  seek_out oc pos;
  output_string oc (string_of_task (update_task_field task data field));
  close_out oc

(* Problems:
   - If the new data is not the exact length of the old data then the
     update will corrupt itself and/or surrounding data.
   - 
*)