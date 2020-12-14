type user_access = 
  | Manager
  | Engineer
  | Scrummer

type t =
  [
    | `ID of int
    | `User of string
    | `Title of string
    | `Status of string
    | `Description of string
    | `TeamName of string
    | `Member of string * user_access
    | `Entry of t list
    | `Password of string
  ]

(** [user_access_of_string s] is [s] converted to a role type. *)
val user_access_of_string : string -> user_access

(** [string_of_user_access r] is [r] converted to a string, as
    represented in the database. *)
val string_of_user_access : user_access -> string

(** [equal a b] is the monomorphic equality of two field types. *)
val equal : t -> t -> bool