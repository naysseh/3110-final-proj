(*Implementation of all frontend to database operations on a user*)

(********Types********)
type user_access = 
  | Manager
  | Engineer
  | Scrummer

type user = {tasks : Database.task list; team : Database.team; 
             role : user_access}
(********Types********)

(********Exceptions********)
exception User_Not_In_Team of string
(********Exceptions********)

(********General Helpers********)
let manager_task_write assignee task_data (team : Database.team) = 
  if List.mem assignee team.members then 
    let task_to_write = 
      assignee :: task_data in
    Database.add_data_all (team.team_name ^ "_issues.txt") task_to_write true
  else raise (User_Not_In_Team assignee)
(********General Helpers********)

let add_task_data task_data user assignee = 
  match user.role with
  | Manager -> manager_task_write assignee task_data user.team
  | Engineer -> "shouldnt happen"
  | Scrummer -> failwith ""