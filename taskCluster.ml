open Cluster

module TaskCluster : MakeCluster = functor (E : EntryType) -> functor (S : Schema) -> struct

  module Entry = E
  module Sch = S
  type id = Entry.id
  type field = Entry.field
  type entry = Entry.t

  exception NotFound of string

  (* TODO: parametrize cluster by team! *)
  let filename = "issues.txt"
  let entry_sep = Sch.entry_sep

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

  let add = failwith "Unimplemented"

  let delete = failwith "Unimplemented"

  let update = failwith "Unimplemented"
end