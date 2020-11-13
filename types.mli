open Cluster

type team = {teamname: string; members: string list}
type task = {id: int; assignee: string; title: string; status: string; description: string}

module Task : EntryType with type t = task
module Team : EntryType with type t = team