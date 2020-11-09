(*Implementation of all frontend to database operations on a user*)
open Database


(********Types********)
type user_access = 
  | Manager
  | Engineer
  | Scrummer

type user = {tasks : task list; teams : team list; 
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
(********General Helpers********)

let create_session username = 
  let tasks = List.filter (fun x -> x.assignee = username) 
      (Database.search_tasks username) in
  let team_match = List.filter (fun x -> List.mem username x.members) (Database.search_teams username) in
  {tasks = tasks; teams = team_match; role = Engineer}

let add_task_data task_data user assignee = 
  match user.role with
  (*| Manager -> manager_task_write assignee task_data user.teams*)
  | Engineer -> failwith ""
  | Scrummer -> failwith ""

let rec check_logins logins username = 
  match logins with
  | [] -> raise (NotFound username)
  | h::t -> if username = h.username then h.password
    else check_logins t username

let log_in username = 
  let results = search_logins username in
  match results with 
  | logins -> check_logins logins username