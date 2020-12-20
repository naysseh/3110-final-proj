open OUnit2
open Database

(********************************************************************
   Here, you can put helper functions to test our modules. 
 ********************************************************************)
let search_tasks_test (name : string) (criteria : string) 
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let search_tasks_with_add_test (name : string) (criteria : string) 
    (add : string list) (expected_output) = 
  name >:: (fun _ -> 
      add_data_all "issues.txt" add true;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let search_tasks_with_delete_test (name : string) (criteria : string) 
    (add : int) (expected_output) = 
  name >:: (fun _ -> 
      delete_task add;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let search_teams_test (name : string) (criteria : string)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_teams criteria)))

let search_tasks_with_edit_test (name : string) (criteria : string) change
    field id (expected_output) = 
  name >:: (fun _ -> 
      edit_task change field id;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let login_test (name : string) (user: string) (err_exp : bool) expected_out = 
  name >:: (fun _ -> 
      if err_exp then
        assert_raises (Database.NotFound name) (fun () -> User.log_in name)
      else 
        assert_equal expected_out (User.log_in user))


(********************************************************************
   End helper functions.
 ********************************************************************)

let database_tests =
  [
    search_tasks_test "search for Andrii" "Andrii" 
      [{Database.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Database.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_test "search for o" "o" 
      [{Database.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Database.id = 4; assignee = "Clarkson"; title = "Lecture"; status = "To do";
        description = "\"All the time\""};
       {Database.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Database.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""};
       {Database.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_test "search for Nat" "Nat" 
      [{id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""}];
    search_teams_test "search for potter" "Potter" 
      [{Database.team_name = "Gryffindor";
        members = ["Manager Potter"; "Scrummer Hermione"; 
                   "Engineer Ron"; "Engineer Ginny"]}];
    search_teams_test "teams search for o" "o" 
      [{team_name = "3110 heroes";
        members = ["Manager Andrii"; "Manager Brady"; "Manager Natasha"]};
       {team_name = "best profs ever";
        members = ["Scrummer Clarkson"; "Engineer White"; "Manager Gries"]};
       {team_name = "Gryffindor";
        members =
          ["Manager Potter"; "Scrummer Hermione"; 
           "Engineer Ron"; "Engineer Ginny"]};
       {team_name = "Slytherin";
        members =
          ["Manager Salazar"; "Manager Voldemort"; "Engineer Blaze"; "Scrummer Draco";
           "Engineer Crabb"; "Engineer Goyle"]}];
    search_tasks_with_add_test "adding task to sleep more" "sleep" 
      ["Gries"; "Sleep"; "In development"; "\"just sleep\""]
      [{Database.id = 6; assignee = "Gries"; title = "Sleep";
        status = "In development"; description = "\"just sleep\""};
       {Database.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""}];
    search_tasks_with_delete_test "deleting Gries task" "sleep" 6
      [{id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""}];
    search_tasks_with_edit_test "make Andrii jump" "Jump" "Jump" "title" 1
      [{id = 1; assignee = "Andrii"; title = "Jump"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_with_edit_test "make Andrii yeet again" "Yeet" "Yeet" "title" 1
      [{id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
  ]

let create_session_test name username expected_user = 
  name >:: (fun _ -> 
      assert_equal expected_user (User.create_session username))

let get_team_tasks_test name (team : Types.team) expected_tasks = 
  name >:: (fun _ -> 
      assert_equal expected_tasks (User.get_team_tasks team))

let get_task_by_id_test name tasks id exp_task exc_exp = 
  name >:: (fun _ -> 
      if exc_exp then
        assert_raises (Not_found) (fun () -> User.get_task_by_id tasks id)
      else 
        assert_equal exp_task (User.get_task_by_id tasks id))

let get_team_test name team_name exp_team exc_exp = 
  name >:: (fun _ -> 
      if exc_exp then
        assert_raises (NotFound team_name) (fun () -> User.get_team team_name)
      else 
        assert_equal exp_team (User.get_team team_name))

let man_task_write_test name username data team tasks expected_tasks 
    expected_search = 
  name >:: (fun _ -> 
      let x = User.manager_task_write username data team tasks and
      y = search_tasks username in
      assert_equal (expected_tasks, expected_search) 
        (x, y))

let man_task_edit_test name id field value tasks expected_tasks 
    expected_search = 
  name >:: (fun _ -> 
      let x = User.manager_task_edit id field value tasks and
      y = search_tasks value in
      assert_equal (expected_tasks, expected_search) 
        (x, y))

let man_task_rem_test name id tasks crit expected_tasks expected_search = 
  name >:: (fun _ -> 
      let x = User.manager_task_remove id tasks and
      y = search_tasks crit in
      assert_equal (expected_tasks, expected_search) 
        (x, y))

let backend_tests = 
  [
    login_test "existing user" "test" false "test12345";
    login_test "nonexisting" "nope" true "lol";
    login_test "natasha" "Natasha" false "passwordlol";
    login_test "nonexisting user within a team" "Salazar" true "slytherin";
    create_session_test "Natasha user" "Natasha"  
      {User.tasks =
         [{Types.id = 2; assignee = "Natasha"; 
           title = "Sleep"; status = "Active";
           description = 
             "\"natasha is tired after 3110 and just wants to sleep\""}];
       teams =
         [{Types.teamname = "3110 heroes";
           members =
             [("Andrii", Field.Manager); ("Brady", Field.Manager);
              ("Natasha", Field.Manager)]}];
       role = Field.Manager};
    create_session_test "Andrii two tasks" "Andrii"
      {User.tasks =
         [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
           description = "\"yeet yote yeeten\""};
          {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
           status = "To do"; description = "\"New CIS college ey\""}];
       teams =
         [{Types.teamname = "3110 heroes";
           members =
             [("Andrii", Field.Manager); ("Brady", Field.Manager);
              ("Natasha", Field.Manager)]}];
       role = Field.Manager};
    get_team_tasks_test "3110 heroes" 
      {Types.teamname = "3110 heroes"; 
       members =
         [("Andrii", Field.Manager); ("Brady", Field.Manager);
          ("Natasha", Field.Manager)]} 
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""};
       {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""}];
    get_task_by_id_test "task with id 3" 
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""};
       {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""}] 3 
      {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
       description = "\"brady just wants to code\""} false;
    get_task_by_id_test "error" [] 2 
      {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
       description = "\"brady just wants to code\""} true;
    get_team_test "non existent" "nah" 
      {Types.teamname = "3110 heroes"; members = []} true;
    get_team_test "Gryffindor people" "Gryffindor" 
      {Types.teamname = "Gryffindor";  
       members =
         [("Potter", Field.Manager); ("Hermione", Field.Scrummer);
          ("Ron", Field.Engineer); ("Ginny", Field.Engineer)]} false;
    man_task_write_test "Andrii add task" "Andrii" 
      ["Swim"; "Active"; "\"Just Swim Bro\""] 
      {Types.teamname = "3110 heroes"; 
       members =  [("Andrii", Field.Manager)]} [] 
      [{Types.id = 6; assignee = "Andrii"; title = "Swim"; status = "Active";
        description = "\"Just Swim Bro\""}] 
      [{Database.id = 6; assignee = "Andrii"; title = "Swim"; status = "Active";
        description = "\"Just Swim Bro\""};
       {Database.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Database.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    man_task_edit_test "Edit swim to fly" 6 "title" "Fly" 
      [{Types.id = 6; assignee = "Andrii"; title = "Swim"; status = "Active";
        description = "\"Just Swim Bro\""}] 
      [{Types.id = 6; assignee = "Andrii"; title = "Fly"; status = "Active";
        description = "\"Just Swim Bro\""}]
      [{Database.id = 6; assignee = "Andrii"; title = "Fly"; status = "Active";
        description = "\"Just Swim Bro\""};
       {Database.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Database.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    man_task_rem_test "Delete the flying" 6 
      [{Types.id = 6; assignee = "Andrii"; title = "Fly"; status = "Active";
        description = "\"Just Swim Bro\""}] "Fly" [] [];
  ]

module TaskCluster = MakeCluster.MakeCluster(Types.Task)(Cluster.NumIDSchema)

let checker (qfunc : Field.t) : bool = 
  if qfunc = `User "lol" then true else false

let cluster_search_tasks_test (name : string) (criteria : string) 
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let cluster_search_tasks_with_add_test (name : string) (criteria : string) 
    (add : string list) (expected_output) = 
  name >:: (fun _ -> 
      add_data_all "issues.txt" add true;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let cluster_search_tasks_with_delete_test (name : string) (criteria : string) 
    (add : int) (expected_output) = 
  name >:: (fun _ -> 
      delete_task add;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let cluster_search_teams_test (name : string) (criteria : string)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_teams criteria)))

let cluster_search_tasks_with_edit_test (name : string) (criteria : string) change
    field id (expected_output) = 
  name >:: (fun _ -> 
      edit_task change field id;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let cluster_task_tests = 
  [

  ]

let suite =
  "test suite for MS1"  >::: List.flatten [
    backend_tests;
    database_tests;
  ]

let _ = run_test_tt_main suite