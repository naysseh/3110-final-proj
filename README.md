# TRAKIO
CS 3110 final project for ai93, nca28, bas339

Description: Trakio is the framework for an Agile workflow management system written entirely in OCaml. We created a UI allowing users to “log in” with their saved data and interact with their tasks and teams. We have created a highly flexible database, one that is able to store any type of data needed by a potential client, via a provided Functor. 

Key Features:
  * Tasks (issues) - stored in issues.txt: 
    * Have descriptions.
    * Ability to add, delete, edit, depending on the access permissions (roles).
  * Fields of tasks:
    * ID
    * Assignee
    * Title
    * Status
    * Description
  * Users - shown at top 
    * Users have a name and role, and belong to a team.
    * Log in through saved username and password currently stored in login_details.txt. The username is the same as the name of the user in a team. 
  * Roles - Manager, Scrummer, Engineer
   * Manager: may add, edit (based on field), and delete tasks. May also add members to their team.
   * Scrummer/Engineer: may view all the tasks on their team. No editing capabilities. 
   * Users are connected to their role in a team as stored in the teams.txt file. (The format is [Role] [Name] where a user's name follows the (capitalized) role.)
  * Query Language - created so that other developers may seamlessly integrate this code into their own. 

Inside INSTALL.txt, there are instructions on how to demo this code - you can log in as a Manager and explore the functionality there, as well as through an Engineer. While logged in as a manager, a user may add another user into their team (ADD > MEMBER) to see how user information is added into our saved files. (For reference, the command to start the REPL in command-line is "make start"). 

Tests for this project are written in test.ml. The test plan is described in detail at the top of that file - run with "make test". 
