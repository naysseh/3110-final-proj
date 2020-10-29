open Cluster

module Make : MakeCluster = functor (E : EntryType) -> struct

  module Entry = E
  type id = Entry.id
  type entry = Entry.t

  type search_result = 
    | Success of string list
    | Unsuccessful of string

  exception NotFound of string

  let filename = "issues.txt"

  let rep_ok = failwith "Unimplemented"

  let search criterion = failwith "Unimplemented"
  (* match get_search_results filename criterion with
     | Success x -> form_task_list x
     | Unsuccessful x -> raise (NotFound criterion) *)

  let add = failwith "Unimplemented"

  let delete = failwith "Unimplemented"

  let update = failwith "Unimplemented"
end