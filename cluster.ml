module type EntryType = sig
  type id
  type t
  type field
  val create_entry : string list -> t
  val string_of_entry : t -> string list
end

module type Schema = sig
  val deserialize : string -> string list
  val serialize : string list -> string
  val search : string -> string -> string list option
  val add : string -> string -> bool
  val modify : string -> int -> (string -> int -> out_channel -> unit) -> bool
end

(** [GeneralSchema] is the common file architecture Trakio uses. *)
module GeneralSchema : Schema = struct

  (** TODO: replace with a regex checker *)
  let string_contains str1 str2= 
    let len1 = String.length str1 and len2 = String.length str2 in
    if len1 < len2 then false else
      let rec check i = 
        if i > len1 - len2 then false else
        if (str2 = String.sub str1 i len2) then true else check (succ i) in
      check 0

  let deserialize line =
    String.split_on_char ';' line

  let serialize data =
    String.concat ";" data

  let search filename criterion = 
    let channel = open_in filename in
    let rec parse_line chnl acc= 
      try let x = input_line chnl in
        if string_contains x criterion then parse_line chnl (x :: acc) 
        else parse_line chnl acc
      with End_of_file -> acc in
    let results = parse_line channel [] in
    close_in channel;
    if List.length results != 0 then Some results 
    else None

  let modify
      (filename : string)
      (start_id : int)
      (mod_line : string -> int -> out_channel -> unit) : bool =
    let ic = open_in filename in
    let temp = filename ^ ".temp" in
    let oc = open_out temp in
    let rec process i =
      try let line = input_line ic in
        mod_line line i oc; process (pred i)
      with End_of_file ->
        begin
          flush oc;
          close_in ic;
          close_out oc;
          Sys.remove filename;
          Sys.rename temp filename
        end in
    try process start_id; true
    with Sys_error e -> false

  let add filename entry = 
    let temp_file = filename ^ ".temp" in
    let ic = open_in filename in
    let oc = open_out temp_file in
    output_string oc entry;
    let rec add_line () = 
      try let line = input_line ic in
        output_char oc '\n'; output_string oc line;
        add_line ()
      with End_of_file ->
        begin 
          flush oc;
          close_in ic;
          close_out oc;
          Sys.remove filename;
          Sys.rename temp_file filename
        end in
    try add_line (); true
    with Sys_error e -> false
end

module type Cluster = sig
  module Entry : EntryType
  module Sch : Schema

  type id = Entry.id
  type field = Entry.field
  type entry = Entry.t

  exception NotFound of string

  val filename : string
  val search : string -> entry list
  val delete : id -> bool
  val add : string list -> bool
  val update : id -> field -> bool
end

module type MakeCluster = functor (E : EntryType) -> functor (S : Schema) ->
  Cluster with module Entry = E and module Sch = S