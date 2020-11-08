open Cluster

module Task : EntryType = struct

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

  let update_field field t =
    match field with
    | `ID id -> {t with id=id}
    | `Assignee name -> {t with assignee=name}
    | `Title title -> {t with title=title}
    | `Status status -> {t with status=status}
    | `Description desc -> {t with description=desc}

  let to_list t =
    [string_of_int t.id; t.assignee; t.title; t.status; t.description]
end