(** Database cluster storing tasks. *)

open Cluster

module Task : EntryType with type id = int
module NumIDSchema : Schema with type id = int

(** [TaskCluster] makes a [Cluster] that stores
    tasks as entries within our general file structure. *)
module TaskCluster : MakeCluster