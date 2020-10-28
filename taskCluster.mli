(** Database cluster storing tasks. *)

open Cluster

(** [Make] make a [Cluster] that stores
    tasks as entries. *)
module Make : MakeCluster