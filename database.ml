(*Implementation of the data file management*)

(*These data types are just drafts to implement basic functionality, 
  change as you will*)
type task = {id : int; assignee : string; title : string; status : string; description : string}
type team = {team_name : string; members : string list}

exception InvalidFile of string
exception Not_found of string

type task_search_result = 
  | Success of task list
  | Unsuccessful of string

let string_contains str1 str2= 
  let len1 = String.length str1 and len2 = String.length str2 in
  if len1 < len2 then false else
    let rec check i = 
      if i > len1 - len2 then false else
      if (str2 = String.sub str1 i len2) then true else check (succ i) in
    check 0

let get_task_data filename search : task_search_result=
  let create_task string = 
    match String.split_on_char ';' string with
    | id::assignee::title::status::description -> 
      {id = int_of_string id; assignee = assignee; title = title; status = status; description = List.hd description}
    | _ -> failwith "mistake with the reading" in
  let channel = open_in filename in
  let rec parse_line chnl acc= 
    match input_line chnl with 
    | x -> if string_contains x search then parse_line chnl (create_task x :: acc) 
      else parse_line chnl acc
    | exception End_of_file -> acc in
  let search_results = parse_line channel [] in
  close_in channel;
  if List.length search_results != 0 then Success search_results 
  else Unsuccessful ("Could not find anything matching: " ^ search)

let add_data filename data = 
  let channel = open_out_gen [Open_append] 0o640 filename in
  output_string channel ("\n" ^ data); 
  close_out channel

(* this just converts given task to string form so i could use in file *)
let record_to_string (task : task) : string = 
  string_of_int task.id ^ ";" ^ task.assignee ^ ";" ^ task.title ^ ";" ^ task.status ^ ";"
  ^ "\"" ^ task.description ^"\""

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
  output_string channel ("\n" ^ record_to_string(
      {id = new_id; assignee = new_assignee; title = new_title; 
       status =  new_status; description = new_descr})); 
  close_out channel

(* Right now, this just overwrites the whole file, so DON'T USE IT UNLESS YOUR
   DATA IS BACKED UP!! I'm working on what to do next :) *)
let update_data filename predicate field data =
  let channel = open_out filename in
  output_string channel data;
  close_out channel