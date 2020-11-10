(** Database cluster storing tasks. *)

open Cluster

(** [Make] makes a [Cluster] that stores
    entries within a given file structure. *)
module Make : MakeCluster