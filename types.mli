open Cluster
open Field

type team = {teamname: string; members: (string * user_access) list}
type task = {id: int; 
             assignee: string; 
             title: string; 
             status: string; 
             description: string}
type login = {username: string; password: string}

module Task : EntryType with type t = task
module Team : EntryType with type t = team
module Login : EntryType with type t = login