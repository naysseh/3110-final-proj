open Cluster

module Task : EntryType = struct

  type t = {id: int; assignee: string; title: string; status: string; description: string}

  let assoc_file = "issues.txt"

  let create_entry = function
    | id::assignee::title::status::description -> 
      {id = int_of_string id; assignee = assignee; title = title; 
       status = status; 
       description =
         if List.length description = 1 && List.hd description = ""  then "" else
           let temp_descr = List.fold_left (fun x y -> x ^ ";" ^ y) "" description in
           String.sub temp_descr 1 ((String.length temp_descr) - 1)}
    | _ -> failwith "mistake with creating a task"

  let update_field field t =
    match field with
    | `ID id -> {t with id=id}
    | `User name -> {t with assignee=name}
    | `Title title -> {t with title=title}
    | `Status status -> {t with status=status}
    | `Description desc -> {t with description=desc}
    | _ -> t

  let to_string_list t =
    [string_of_int t.id; t.assignee; t.title; t.status; t.description]

  let to_field_list t =
    [`ID t.id; `User t.assignee; `Title t.title; `Status t.status; `Description t.description]
end

module Team : EntryType = struct

  let assoc_file = "teams.txt"

  type t = {teamname: string; members: string list}

  let create_entry = function 
    | h::t -> {teamname = h; members = t}
    | _ -> failwith "mistake with creating a team"

  let update_field field t =
    match field with
    | `TeamName name -> {t with teamname=name}
    | `Members users -> {t with members=users}
    | _ -> t (* Field does not exist. To not update, or to raise an exception? *)

  let to_string_list t = t.teamname :: t.members

  let to_field_list t = [`TeamName t.teamname; `Members t.members]
end