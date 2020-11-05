open Cluster

module Task : EntryType = struct
  type id = int
  (* Might get rid of the field type as a requirement for EntryType. Idk. *)
  type field =
      ID of id
    | Assignee of string
    | Title of string
    | Status of string
    | Description of string
  type t = {id: int; assignee: string; title: string; status: string; description: string}

  let create_entry = function
    | id::assignee::title::status::description -> 
      {id = int_of_string id; assignee = assignee; title = title; 
       status = status; 
       description =
         if List.length description = 1 && List.hd description = ""  then "" else
           let temp_descr = List.fold_left (fun x y -> x ^ ";" ^ y) "" description in
           String.sub temp_descr 1 ((String.length temp_descr) - 1)}
    | _ -> failwith "mistake with the reading"

  let string_of_entry task =
    [string_of_int task.id; task.assignee; task.title; task.status; "\"" ^ task.description ^ "\""]
end

module TaskCluster : MakeCluster = functor (E : EntryType) -> functor (S : Schema) -> struct

  module Entry = E
  module Sch = S
  type id = Entry.id
  type field = Entry.field
  type entry = Entry.t

  exception NotFound of string

  (* TODO: parametrize cluster by team! *)
  let filename = "issues.txt"

  let rep_ok = failwith "Unimplemented"

  let total_tasks () = 
    let chnl = open_in filename in
    let top_line = input_line chnl in
    close_in chnl;
    match String.split_on_char ';' top_line with
    | [] -> 0
    | h::_ -> int_of_string h

  let form_list (l : string list) : entry list =
    List.map Sch.deserialize l
    |> List.map Entry.create_entry

  let search criterion =
    match Sch.search filename criterion with
    | Some x -> form_list x
    | None -> raise (NotFound criterion)

  (* TODO: Check data is valid *)
  let add data = Sch.add (Sch.serialize data) filename

  let dec_id task_line =
    let delim = String.index task_line ';' in
    let id = int_of_string (String.sub task_line 0 delim) in
    let rest = String.sub task_line delim (String.length task_line - delim) in
    string_of_int (id - 1) ^ rest

  let delete id = failwith ""
  (* let incl line i oc =
     if i <> id then
      begin
        let out_line = if i > id then dec_id line else line in
        output_string oc out_line;
        (* If i is 1, then don't make a new line.
           If i is 2, and del is 1, then don't make a new line. *)
        if not (i = 1 || (id = 1 && i = 2)) then output_char oc '\n'
      end
     in Sch.modify filename (total_tasks ()) incl *)

  let update = failwith "Unimplemented"
end