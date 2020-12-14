open Cluster

module MakeCluster : MakeCluster = 
  functor (E : EntryType) -> 
  functor (S : Schema) -> struct

    module Entry = E
    module Sch = S
    type entry = Entry.t

    let filename = ref E.assoc_file

    let bind teamname = filename := (teamname ^ "_" ^ E.assoc_file)
    let unbind () = filename := E.assoc_file

    let verify line =
      Sch.deserialize line
      |> Entry.create_entry
      |> Entry.to_string_list
      |> Sch.serialize

    let rep_ok () = Sch.rep_ok ~aux:verify !filename

    let form_list (l : string list) : entry list =
      List.map Sch.deserialize l
      |> List.map Entry.create_entry

    let check (ctxt, criterion) entry =
      Entry.to_field_list entry
      |> match ctxt with
      | Sloppy -> List.exists criterion
      | Strict -> List.for_all criterion

    let select ctxt_criterion =
      let checker line =
        check ctxt_criterion (Entry.create_entry (Sch.deserialize line))
      in Sch.search !filename checker

    let search ctxt_criterion =
      match select ctxt_criterion with
      | None -> []
      | Some x -> form_list x

    (* TODO: Check data is valid *)
    let add data = Sch.add !filename (Sch.serialize data)

    (* There's something easy to do for the ID-based schema: simply accept IDs
       as they are known by the user and delete with that. No need to do a 
       selection first. 
       Also, we can try to assess if a lot of lines are going to be deleted by
       some condition. If so, then we can flip that condition and do a *keep*
       -type of operation instead of a delete (less resource-intensive). *)
    let delete ctxt_criterion =
      match select ctxt_criterion with
      | None -> Ok 0
      | Some l -> Sch.delete !filename (List.rev l)

    let update field ctxt_criterion =
      let new_line upd line =
        let modify entry = 
          if check ctxt_criterion entry 
          then Entry.update_field upd entry
          else entry
        in modify (Entry.create_entry (Sch.deserialize line))
           |> Entry.to_string_list
           |> Sch.serialize
      in Sch.update !filename (new_line field)
  end