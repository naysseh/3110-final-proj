open OUnit2
open Database

(********************************************************************
   Here, you can put helper functions to test our modules. 
 ********************************************************************)
let search_tasks_test (name : string) (criteria : string) 
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (search_tasks criteria))

(********************************************************************
   End helper functions.
 ********************************************************************)

let database_tests =
  [
    search_tasks_test "search for Andrii" "Andrii" 
      [{id = 3; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_test "search for o" "o" 
      [{id = 1; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""};
       {id = 3; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_test "search for Nat" "Nat" 
      [{id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""}];
  ]

let suite =
  "test suite for MS1"  >::: List.flatten [
    database_tests;
  ]

let _ = run_test_tt_main suite