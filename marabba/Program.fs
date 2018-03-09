open System.Security.Cryptography

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Token =
| P             //different colours 
| O
| Blank

type Grid_list =
| Grid of (Token*Token*Token)*(Token*Token*Token)*(Token*Token*Token)*(Token*Token*Token*Token*Token*Token)*(Token*Token*Token)*(Token*Token*Token)*(Token*Token*Token)

type progress =
| Ongoing of Grid_list
| Winner of Token * Grid_list
| Draw

let initialise = 
   let blankrow = Blank, Blank, Blank
   let blankrow2 = Blank, Blank, Blank,Blank, Blank, Blank
   Grid (blankrow, blankrow, blankrow, blankrow2, blankrow, blankrow, blankrow)

let printboard (b:string list) =     //passing the list in
  printfn " %s--------%s--------%s
              %s---%s---%s
                %s-%s-%s
           %s-%s-%s      %s-%s-%s
                %s-%s-%s
              %s---%s---%s
           %s--------%s--------%s" b.[0]  b.[1]  b.[2]  b.[3] b.[4] b.[5] b.[6] b.[7] b.[8] b.[9] b.[10] b.[11] b.[12] b.[13]  b.[14] b.[15] b.[16] b.[17] b.[18] b.[19] b.[20] b.[21] b.[22] b.[23]
     

let Mill_Formation list player =   // checks if a mill formation occured 
  let result=                       
    match player with
    | P ->
          match list with 
          | ["P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]   //horizontal cases
          | [ _;_;_; "P";"P";"P"; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] 
          | [ _;_; _;_;_;_; "P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]
          | [ _;_; _;_;_;_; _;_;_;"P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_]
          | [ _;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_;_;_;_;_;_;_]
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_;_;_;_]
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_]
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; _;_;_; "P";"P";"P"] 
          | [ "P";_;_;_;_;_;_;_;_; "P";_;_;_;_;_;_;_;_;_;_;"P";_;_;_] //vertical cases
          | [ _;_; _;"P";_;_;_;_;_;_;"P";_;_;_;_;_;_;_;"P";_;_; _;_;_]
          | [ _;_;_;_;_;_; "P";_;_;_;_;"P";_;_;_; "P";_;_;_;_;_;_;_;_]
          | [ _;"P";_;_;"P";_;_;"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P";_]
          | [ _;_; _;_;_;_;_;_;"P";_;_;_;"P";_;_;_;_; "P";_;_;_;_;_;_]
          | [ _;_;_;_;_;"P";_;_;_;_;_;_;_;"P";_;_;_;_;_;_;"P";_;_;_]
          | [ _;_; "P";_;_;_;_;_;_;_;_;_;_;_;"P";_;_;_;_;_;_;_;_; "P"]
          | [ "P";_;_;"P";_;_; "P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] //Diagonal 
          | [ _;_; "P";_;_;"P";_;_;"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P";_;_] | [ _;_; "P";_;_;"P";_;_;"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]  | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P"] -> true
          | _ -> false
    | O -> 
          match list with
          | ["O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]   //horizontal cases
          | [ _;_;_; "O";"O";"O"; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] 
          | [ _;_; _;_;_;_; "O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]
          | [ _;_; _;_;_;_; _;_;_;"O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_]
          | [ _;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_;_;_;_;_;_;_]
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_;_;_;_]
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_]
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; _;_;_; "O";"O";"O"] 
          | [ "O";_;_;_;_;_;_;_;_; "O";_;_;_;_;_;_;_;_;_;_;"O";_;_;_] //vertical cases
          | [ _;_; _;"O";_;_;_;_;_;_;"O";_;_;_;_;_;_;_;"O";_;_; _;_;_]
          | [ _;_;_;_;_;_; "O";_;_;_;_;"O";_;_;_; "O";_;_;_;_;_;_;_;_]
          | [ _;"O";_;_;"O";_;_;"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O";_]
          | [ _;_; _;_;_;_;_;_;"O";_;_;_;"O";_;_;_;_; "O";_;_;_;_;_;_]
          | [ _;_;_;_;_;"O";_;_;_;_;_;_;_;"O";_;_;_;_;_;_;"O";_;_;_]
          | [ _;_; "O";_;_;_;_;_;_;_;_;_;_;_;"O";_;_;_;_;_;_;_;_; "O"]
          | [ "O";_;_;"O";_;_; "O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] //Diagonal 
          | [ _;_; "O";_;_;"O";_;_;"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O";_;_] | [ _;_; "O";_;_;"O";_;_;"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]  | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O"] -> true
          | _ -> false
          
//let b = [ "a1";"a4"; "a7";"b2";"b4";"b6"; "c3";"c4";"c5"; "d1";"d2";"d3";"d5";"d6";"d7"; "e3"; "e4"; "e5";"f2";"f4";"f6"; "g1"; "g4"; "g7"]
let shooting_occur list player  =    //shooting another cow function
  printfn  "%A's turn.  Type the number with the cow you want to shoot." player  //read the user input on which cow they want to kill 
  let n = System.Console.Readline()
      let rec transversal inputlist outputlist
         let opponent=                       //checks who is shooting who 
             match player with
             | P -> "O"
             | O -> "P"
         match inputlist with               // gets rid of the person's cow in the list and gives back a new list. 
          | [] -> List.rev outputlist
          | x:: rest -> 
                       match opponent = x with
                       | true -> transversal rest n:: outputlist
                       | _    -> transversal rest outputlist
      transversal list []





let adding_input (n: string) (list:string list) player =      //takes the inputread, the list, and the player 
 let whose_turn=
  match player with
   | P ->"p"
   | O -> "O"
 let rec match_match n (input_list: string list) output_list (turn: string)  =
   match input_list with
   |[]-> List.rev output_list
   |x::rest ->
     match n = x with 
     | true-> match_match n rest turn::output_list 
     |_ -> match_match n rest  x::output_list 
 match_match n list [] whose_turn 


let playing_initiation =
   let b = [ "a1"; "a4"; "a7";"b2";"b4";"b6"; "c3";"c4";"c5"; "d1";"d2";"d3";"d5";"d6";"d7"; "e3"; "e4"; "e5";"f2";"f4";"f6"; "g1"; "g4"; "g7"]
   printboard b
   printfn  "%A's turn.  Type the number of the cell that you want to play into." player
   let _n = System.Console.Readline()
   adding_input n b player



[<EntryPoint>]
let main argv = 
  
  
    System.Console.ReadKey()|>ignore
    0 // return an integer exit code
