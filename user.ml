(*Implementation of all frontend to database operations on a user*)
open MakeCluster
open Cluster
open Field

module Teams = MakeCluster (Types.Team) (NoIDSchema)
module Tasks = MakeCluster (Types.Task) (NumIDSchema)
module LoginBase = MakeCluster (Types.Login) (NoIDSchema)

(********Types********)
type user = {tasks : Types.task list; teams : Types.team list; 
             role : user_access}
(********Types********)

(********Exceptions********)
exception User_Not_In_Team of string
exception Database_Fatal_Error of string
(********Exceptions********)

(********General Helpers********)
let user_in_team username (team : Types.team) = 
  List.fold_left (fun b (name, _) -> b || (name = username)) false team.members

let append_task data tasks = 
  let id = 1 + List.fold_left (fun max_id (task : Types.task) -> 
      if task.id > max_id then task.id else max_id) 0 tasks in
  let sorter (t1 : Types.task) (t2 : Types.task) = 
    if t1.id > t2.id then 1 else
    if t1.id = t2.id then 0 else -1 in
  let (task : Types.task) = match data with
    | assignee :: title :: status :: description :: _ -> 
      {id = id; assignee = assignee; title = title;
       status = status; description = description} 
    | _ -> failwith "" in
  List.sort sorter (task :: tasks)

(**add change of tasks list*)
let manager_task_write assignee task_data team tasks =
  if user_in_team assignee team then
    let task_to_write = 
      assignee :: task_data in
    match Tasks.add task_to_write with
    | Ok i -> append_task task_data tasks
    | Error s -> raise (Database_Fatal_Error s)
  else raise (User_Not_In_Team assignee)

let manager_task_remove id tasks =
  let remover (task : Types.task) = 
    task.id != id in 
  match Tasks.delete (Strict, function | `ID i -> i = id | _ -> true) with
  | Ok i -> if i = 1 then List.filter remover tasks else tasks
  | Error s -> raise (Database_Fatal_Error s)

let manager_task_edit field new_val tasks =
  let to_update = List.map (fun (t : Types.task) -> t.id) tasks in
  let field = Field.make_str_field field new_val in
  Tasks.update field 
    (Sloppy, function | `ID id -> List.mem id to_update
                      | _ -> false)

let by_user username = 
  Strict, function | `User s -> s = username | _ -> true

let contains_user username =
  Sloppy, function | `Member (s, _) -> s = username | _ -> false

let by_teamname teamname = 
  Strict, function | `TeamName s -> s = teamname | _ -> true

(** Note that this helper operates on the assumption of one team and one role
    per user. *)
let recover_team_role username (teams : Types.team list) =
  match teams with
  | [] -> raise (User_Not_In_Team username)
  | team::_ as l -> (l, List.assoc username team.members)
(********General Helpers********)

let create_session username = 
  let tasks =
    by_user username |> Tasks.search in
  let team_match, role = 
    contains_user username |> Teams.search |> recover_team_role username in
  {tasks = tasks; teams = team_match; role = role}

let add_task_data task_data user assignee = 
  match user.role with
  | Manager -> failwith ""
  | Engineer -> failwith ""
  | Scrummer -> failwith ""

let rec check_logins (logins : Types.login list) username = 
  match logins with
  | [] -> raise (Database.NotFound username)
  | h::t -> if username = h.username then h.password
    else check_logins t username

let log_in username =
  let results = by_user username |> LoginBase.search in
  match results with 
  | logins -> check_logins logins username

let get_team teamname =
  let results = by_teamname teamname |> Teams.search in
  match results with
  | [] -> raise (Database.NotFound teamname)
  | h::[] -> h
  | _ -> failwith "More than one. Choose"