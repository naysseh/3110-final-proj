(*Implementation of the data file management*)

(*These data types are just drafts to implement basic functionality, 
  change as you will*)
type task = {id : int; assignee : string; title : string; description : string}

type team = {team_name : string; members : string list}

type task_search_result = 
  | Success of task
  | Unsuccessful of string

let get_task_data filename search in_descr=
  let channel = open_in filename in
  let rec parse_line chnl search = 
    match input_line chnl with 
    | x -> failwith ""
    | exception (End_of_file) -> Unsuccessful 
                                   ("Unable to find anything that matches: " ^ 
                                    search) in failwith ""

let add_data filename data = 
  let channel = open_out_gen [Open_append] 0o640 filename in
  output_string channel ("\n" ^ data); 
  close_out channel;