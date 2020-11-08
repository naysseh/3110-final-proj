(** Database cluster storing tasks. *)

open Cluster

module NumIDSchema : Schema

(** [TaskCluster] makes a [Cluster] that stores
    tasks as entries within our general file structure. *)
module TaskCluster : MakeCluster