open OUnit2
open Database

(********************************************************************
   Here, you can put helper functions to test our modules. 
 ********************************************************************)



(********************************************************************
   End helper functions.
 ********************************************************************)

let database_tests =
  [
    (* add tests here *)
  ]

let suite =
  "test suite for MS1"  >::: List.flatten [
    database_tests;
  ]

let _ = run_test_tt_main suite