# TRAKIO
CS 3110 final project for ai93, nca28, bas339

Description: Trakio is the framework for an Agile workflow management system written entirely in OCaml. We created a UI allowing users to “log in” with their saved data and interact with their tasks and teams. We have created a highly flexible database, one that is able to store any type of data needed by a potential client, via a provided Functor. 

Key Features:
  * Tasks (issues)
    * Tasks show up as blocks on a day at the starting screen for the user.
    * Have descriptions.
    * Ability to add, delete, edit, depending on the access permissions (based on roles).
  * Aspects of tasks:
    * Assignee
    * Status
    * Description
    * ID
  * Users - shown at top 
    * Users have a name and belong to a team.
  * Roles - Manager, Scrummer, Engineer
  * Query Language
