(*Implementation of all frontend to database operations on a user*)
open Database
open MakeCluster

module Teams = MakeCluster (Types.Team) (Cluster.NoIDSchema)
module Tasks = MakeCluster (Types.Task) (Cluster.NumIDSchema)
module LoginBase = MakeCluster (Types.Login) (Cluster.NoIDSchema)

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
(********Exceptions********)

(********General Helpers********)
let manager_task_write assignee task_data (team : team) = 
  if List.mem assignee team.members then 
    let task_to_write = 
      assignee :: task_data in
    add_data_all (team.team_name ^ "_issues.txt") task_to_write true
  else raise (User_Not_In_Team assignee)

let by_user username = function | `User s -> s = username | _ -> true

let contains_user username = 
  function | `Members l -> List.mem username l | _ -> true
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