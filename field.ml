type t =
  [
    | `ID of int
    | `User of string
    | `Title of string
    | `Status of string
    | `Description of string
    | `TeamName of string
    | `Members of string list
    | `Entry of t list
    | `Password of string
  ]

let rec equal a b =
  match a, b with
  | `ID a, `ID b -> a = b
  | `User a, `User b -> a = b
  | `Title a, `Title b -> a = b
  | `Status a, `Status b -> a = b
  | `Description a, `Description b -> a = b
  | `TeamName a, `TeamName b -> a = b
  | `Members a, `Members b -> List.sort compare a = List.sort compare b
  | `Entry a, `Entry b ->
    (try List.for_all2 equal a b with Invalid_argument _ -> false)
  | _ -> false