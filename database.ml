(*Implementation of the data file management*)


(*These data types are just drafts to implement basic funcitonality, 
  change as you will*)
type task = {id : int; assignee : string; title : string; description : string}

type team = {team_name : string; members : string list}