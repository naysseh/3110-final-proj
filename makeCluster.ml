open Cluster

module Make : MakeCluster = 
  functor (E : EntryType) -> 
  functor (S : Schema) -> struct

    module Entry = E
    module Sch = S
    type entry = Entry.t

    exception NotFound of string

    (* TODO: parametrize cluster by team! *)
    let filename = "teams.txt"

    (* let rep_ok = failwith "Unimplemented" *)

    let form_list (l : string list) : entry list =
      List.map Sch.deserialize l
      |> List.map Entry.create_entry

    let search criterion =
      match Sch.search filename criterion with
      | Some x -> form_list x
      | None -> raise (NotFound criterion)

    (* TODO: Check data is valid *)
    let add data = Sch.add filename (Sch.serialize data)

    let delete id =
      Sch.delete filename id

    let new_line_task field line =
      Sch.deserialize line
      |> Entry.create_entry
      |> Entry.update_field field
      |> Entry.to_list
      |> Sch.serialize

    let update id field = Sch.update filename id (new_line_task field)
  end