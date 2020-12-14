(*Implementation of all frontend to database operations on a user*)
open Database
open MakeCluster
open Cluster

module Teams = MakeCluster (Types.Team) (NoIDSchema)
module Tasks = MakeCluster (Types.Task) (NumIDSchema)
module LoginBase = MakeCluster (Types.Login) (NoIDSchema)

(********Types********)
type user_access = 
  | Manager
  | Engineer
  | Scrummer

type user = {tasks : Types.task list; teams : Types.team list; 
             role : user_access}
(********Types********)

(********Exceptions********)
exception User_Not_In_Team of string
exception Database_Fatal_Error of string
(********Exceptions********)

(********General Helpers********)
let manager_task_write assignee task_data  =
  let task_to_write = 
    assignee :: task_data in
  match Tasks.add task_to_write with
  | Ok i -> i = 1
  | Error s -> raise (Database_Fatal_Error s)


let by_user username = 
  Strict, function | `User s -> s = username | _ -> true

let contains_user username = 
  Sloppy, function 
    | `Managers l 
    | `Engineers l 
    | `Scrummers l -> List.mem username l 
    | _ -> true

let by_teamname teamname = 
  Strict, function | `TeamName s -> s = teamname | _ -> true
(********General Helpers********)

let create_session username = 
  let tasks = by_user username |> Tasks.search in
  let team_match = contains_user username |> Teams.search in
  {tasks = tasks; teams = team_match; role = Engineer}

let add_task_data task_data user assignee = 
  match user.role with
  (*| Manager -> manager_task_write assignee task_data user.teams*)
  | Engineer -> failwith ""
  | Scrummer -> failwith ""

let rec check_logins (logins : Types.login list) username = 
  match logins with
  | [] -> raise (NotFound username)
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