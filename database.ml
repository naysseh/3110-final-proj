(*Implementation of the data file management*)

(********Types********)
type task = {id : int; assignee : string; title : string; 
             status : string; description : string}
type team = {team_name : string; members : string list}
type search_result = 
  | Success of string list
  | Unsuccessful of string

(********End Types********)

(********Exceptions********)
exception NotFound of string
(********End Exceptions********)

(********Constructors********)
(*Solve the case where you have ; in description, i.e. 
  a helper on List.hd description to link all of em tgth*)
let create_task string = 
  match String.split_on_char ';' string with
  | id::assignee::title::status::description -> 
    {id = int_of_string id; assignee = assignee; title = title; 
     status = status; 
     description =
       if List.length description = 1 && List.hd description = ""  then "" else
         let temp_descr = List.fold_left (fun x y -> x ^ ";" ^ y) "" description in
         String.sub temp_descr 1 ((String.length temp_descr) - 1)}
  | _ -> failwith "mistake with the reading" 

let create_team string = 
  match String.split_on_char ';' string with
  | h::t -> {team_name = h; members = t}
  | _ -> failwith "mistake creating a team"
(********End Constructors********)

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
  string_of_int task.id ^ ";" ^ task.assignee ^ ";" ^ 
  task.title ^ ";" ^ task.status ^ ";" ^ "\"" ^ task.description ^ "\"" 

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

let update_task_field task data = function
  | "id" -> {task with id = int_of_string data; 
                       description = String.sub task.description 1 
                           (String.length task.description - 2)}
  | "assignee" -> {task with assignee=data; 
                             description = String.sub task.description 1 
                                 (String.length task.description - 2)}
  | "title" -> {task with title=data; 
                          description = String.sub task.description 1 
                              (String.length task.description - 2)}
  | "status" -> {task with status=data; 
                           description = String.sub task.description 1 
                               (String.length task.description - 2)}
  | "description" -> {task with description=data}
  | _ -> raise Not_found

let total_tasks filename = 
  let chnl = open_in filename in
  let first_line = input_line chnl in
  match String.split_on_char ';' first_line with
  | [] -> failwith "mistake"
  | h::t -> int_of_string h

let new_line_task old_line change field = 
  let old_task = create_task old_line in
  string_of_task (update_task_field old_task change field)

(*Inneficient, easier to just modify the string*)
let inc_id task_line =
  let task = create_task task_line in
  let change_id id t = update_task_field t id "id" in
  let new_id = string_of_int (pred task.id) in
  change_id new_id task |> string_of_task

let list_to_string data = 
  String.concat ";" data

(********End General Helpers********)

let search_tasks criterion = 
  match get_search_results "issues.txt" criterion with
  | Success x -> form_task_list x
  | Unsuccessful x -> raise (NotFound criterion)

let search_teams criterion =
  match get_search_results "teams.txt" criterion with
  | Success x -> form_teams_list x
  | Unsuccessful x -> raise (NotFound criterion)

let mod_tasks
    (mod_line : string -> int -> 'a -> out_channel -> unit)
    (data : 'a) : unit =
  let ic = open_in "issues.txt" in
  let temp = "issues.txt.temp" in
  let oc = open_out temp in
  let rec process i =
    match input_line ic with
    | line -> mod_line line i data oc; process (pred i)
    | exception (End_of_file) ->
      begin
        flush oc;
        close_in ic;
        close_out oc;
        Sys.remove "issues.txt";
        Sys.rename temp "issues.txt"
      end
  in process (total_tasks "issues.txt")

(*
let edit_oper num_tasks closing inp out id change field= 
  let rec add_line i = 
    match input_line inp with
    | line -> begin
        if i != id then (output_string out line;)
        else output_string out (new_line_task line change field); output_char out '\n';
        add_line (pred i)
      end
    | exception (End_of_file) -> 
      closing in
  add_line num_tasks

let file_operation_generalization oper change field id=
  let total_tasks = total_tasks in
  let temp_file = "issues.txt.temp" in
  let inp = open_in "issues.txt" and 
    out = open_out temp_file in
  (*custom code*)
  let closing = 
    begin 
      flush out;
      close_in inp;
      close_out out;
      Sys.remove "issues.txt";
      Sys.rename temp_file "issues.txt" 
    end in
  oper total_tasks closing inp out

let edit_task change field id = 
  file_operation_generalization (edit_oper) change field id  *)

let add_data filename data  = 
  let new_data = list_to_string data in 
  (* changed total task code here so it updates each time the 
     function is called *)
  let total_tasks =
    let chnl = open_in "issues.txt" in
    let first_line = input_line chnl in
    close_in chnl;
    match String.split_on_char ';' first_line with
    | [] -> failwith "mistake"
    | h::t -> int_of_string h in 
  let temp_file = filename ^ ".temp" in
  let ic = open_in filename and oc = open_out temp_file in 
  let new_task = create_task (string_of_int (total_tasks + 1) ^ ";" ^ new_data) in 
  output_string oc (string_of_task new_task); 
  output_char oc '\n';
  let rec add_line i = 
    match input_line ic with
    | line -> 
      begin
        output_string oc line; 
        if i != 1 then
          output_char oc '\n'; 
        add_line (pred i)
      end 
    | exception (End_of_file) ->
      begin 
        flush oc;
        close_in ic;
        close_out oc;
        Sys.remove filename;
        Sys.rename temp_file filename 
      end in 
  add_line total_tasks

(*For all data types*)
let add_data_all filename data id_required = 
  let temp_file = filename ^ ".temp" in
  let inp = open_in filename and out = open_out temp_file in
  let new_data = if id_required then (string_of_int (total_tasks filename + 1)) 
                                     ^ ";" ^ (String.concat ";" data)
    else String.concat ";" data in
  output_string out new_data; output_char out '\n';
  let rec add_line i = 
    match input_line inp with
    | line -> 
      output_string out line; output_char out '\n'; 
      add_line i;
    | exception (End_of_file) ->
      begin 
        flush out;
        close_in inp;
        close_out out;
        Sys.remove filename;
        Sys.rename temp_file filename; 
      end in 
  add_line 1

(*Review conversion of string to task and back - redundancy and inneficient*)
let add_task data =
  let tot = total_tasks "" in
  let new_task =
    list_to_string data
    |> (fun s -> string_of_int (tot + 1) ^ ";" ^ s)
    |> create_task
    |> string_of_task
    |> (fun s -> if tot > 1 then s ^ "\n" else s) in
  let append line i new_line oc =
    begin
      if i = tot then output_string oc new_line;
      output_string oc line;
      if i > 1 then output_char oc '\n'
    end
  in mod_tasks append new_task


let delete_task id =
  let incl line i del oc =
    if i <> del then
      begin
        let out_line = if i > del then inc_id line else line in
        output_string oc out_line;
        (* If i is 1, then don't make a new line.
           If i is 2, and id is 1, then don't make a new line. *)
        if not (i = 1 || (id = 1 && i = 2)) then output_char oc '\n'
      end
  in mod_tasks incl id

(* let edit_task_data change field id = 
   let num_tasks = total_tasks in
   let temp_file = "issues.temp" in
   let inp = open_in "issues.txt" and 
    out = open_out temp_file in
   let rec add_line i = 
    match input_line inp with
    | line -> begin
        if i != id then (output_string out line;)
        else output_string out (new_line_task line change field); output_char out '\n';
        add_line (pred i)
      end
    | exception (End_of_file) -> 
      flush out;
      close_in inp;
      close_out out;
      Sys.remove "issues.txt";
      Sys.rename temp_file "issues.txt"; in
   add_line num_tasks *)


let edit_task change field id =
  let edit line i (change, field, id) oc =
    begin
      let out_line = if i <> id then line
        else new_line_task line change field
      in output_string oc out_line;
      if i > 1 then output_char oc '\n'
    end
  in mod_tasks edit (change, field, id)