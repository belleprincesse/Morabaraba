open System.Security.Cryptography

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Token =
| P             //different colours 
| O
| Blank

let swapPlayer x =
    match x with
    | P -> O
    | O -> P
    | Blank -> failwith "no player was chosen"


type Grid_list =
| Grid of (Token*Token*Token)*(Token*Token*Token)*(Token*Token*Token)*(Token*Token*Token*Token*Token*Token)*(Token*Token*Token)*(Token*Token*Token)*(Token*Token*Token)  //not sure if we still need this

type progress =    //not sure if we need this 
| Ongoing of Grid_list
| Winner of Token * Grid_list
| Draw

let initialise =    //not sure if we need this 
   let blankrow = Blank, Blank, Blank
   let blankrow2 = Blank, Blank, Blank,Blank, Blank, Blank
   Grid (blankrow, blankrow, blankrow, blankrow2, blankrow, blankrow, blankrow)

let printboard (b:string list) =     //passing the list in
  printfn " 1   2   3      4    5    6   7   
           a %s------------%s-----------%s
             | \           |           / |
           b |  %s---------%s--------%s  |
             |  | \                /  |  |
             |  |  \              /   |  |
             |  |   \            /    |  |
           c |  |    %s---%s---%s     |  |
             |  |    |          |     |  |
           d %s-%s---%s         %s---%s--%s
             |  |    |          |     |  |
           e |  |    %s---%s---%s     |  |
             |  |   /            \    |  |
             |  |  /              \   |  |
             |  | /                \  |  |
           f |  %s--------%s---------%s  |
             | /          |            \ |
           g |%s----------%s------------%s" b.[0]  b.[1]  b.[2]  b.[3] b.[4] b.[5] b.[6] b.[7] b.[8] b.[9] b.[10] b.[11] b.[12] b.[13]  b.[14] b.[15] b.[16] b.[17] b.[18] b.[19] b.[20] b.[21] b.[22] b.[23]
     

let Mill_Formation list player =   // checks if a mill formation occured 
  let result=                       
    match player with
    | P ->
          match list with 
          | ["P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["a1";"a2";"a4"])  //horizontal case
          | [ _;_;_; "P";"P";"P"; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["b2";"b4";"b6"])
          | [ _;_; _;_;_;_; "P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["c3";"c4";"c5"])
          | [ _;_; _;_;_;_; _;_;_;"P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["d1";"d2";"d3"])
          | [ _;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_;_;_;_;_;_;_] -> Some (["d5";"d6";"d7"])
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_;_;_;_]-> Some (["e3"; "e4"; "e5"])
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_]-> Some (["f2";"f4";"f6"])
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; _;_;_; "P";"P";"P"] -> Some (["g1"; "g4"; "g7"])
          | [ "P";_;_;_;_;_;_;_;_; "P";_;_;_;_;_;_;_;_;_;_;_;"P";_;_] -> Some (["a1";"d1";"g1"])   //vertical cases
          | [ _;_; _;"P";_;_;_;_;_;_;"P";_;_;_;_;_;_;_;"P";_;_; _;_;_] -> Some (["b2";"d2";"f2"])
          | [ _;_;_;_;_;_; "P";_;_;_;_;"P";_;_;_; "P";_;_;_;_;_;_;_;_] -> Some (["c3";"d3";"e3"])
          | [ _;"P";_;_;"P";_;_;"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["a4";"b4";"c4"]) 
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P";_] -> Some (["e4";"f4";"g4"])
          | [ _;_; _;_;_;_;_;_;"P";_;_;_;"P";_;_;_;_; "P";_;_;_;_;_;_] -> Some (["c5";"d5";"e5"])
          | [ _;_;_;_;_;"P";_;_;_;_;_;_;_;"P";_;_;_;_;_;_;"P";_;_;_] -> Some   (["b6";"d6";"f6"])
          | [ _;_; "P";_;_;_;_;_;_;_;_;_;_;_;"P";_;_;_;_;_;_;_;_; "P"] -> Some (["a7";"d7";"g7"]) 
          | [ "P";_;_;"P";_;_; "P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]  -> Some (["a1";"b2";"c3"])  //Diagonal  
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P";_;_] -> Some (["e3";"f2";"g1"])
          | [ _;_; "P";_;_;"P";_;_;"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]  -> Some (["a7";"b6";"c5"])
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P"] -> Some (["e5";"f6";"g7"])
          | _ -> None
    | O -> 
          match list with
          | ["O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]  -> Some (["a1";"a2";"a4"])  //horizontal cases
          | [ _;_;_; "O";"O";"O"; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["b2";"b4";"b6"])
          | [ _;_; _;_;_;_; "O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["c3";"c4";"c5"])
          | [ _;_; _;_;_;_; _;_;_;"O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_] -> Some(["d1";"d2";"d3"])
          | [ _;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_;_;_;_;_;_;_] -> Some (["d5";"d6";"d7"])
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_;_;_;_] -> Some (["e3"; "e4"; "e5"])
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_] -> Some (["f2";"f4";"f6"])
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; _;_;_; "O";"O";"O"]  -> Some (["g1"; "g4"; "g7"])
          | [ "O";_;_;_;_;_;_;_;_; "O";_;_;_;_;_;_;_;_;_;_;"O";_;_;_] -> Some (["a1";"d1";"g1"])  //vertical cases
          | [ _;_; _;"O";_;_;_;_;_;_;"O";_;_;_;_;_;_;_;"O";_;_; _;_;_] -> Some (["b2";"d2";"f2"])
          | [ _;_;_;_;_;_; "O";_;_;_;_;"O";_;_;_; "O";_;_;_;_;_;_;_;_] -> Some (["c3";"d3";"e3"])
          | [ _;"O";_;_;"O";_;_;"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["a4";"b4";"c4"]) 
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O";_] -> Some (["e4";"f4";"g4"])
          | [ _;_; _;_;_;_;_;_;"O";_;_;_;"O";_;_;_;_; "O";_;_;_;_;_;_] -> Some (["c5";"d5";"e5"])
          | [ _;_;_;_;_;"O";_;_;_;_;_;_;_;"O";_;_;_;_;_;_;"O";_;_;_] -> Some  (["b6";"d6";"f6"])
          | [ _;_; "O";_;_;_;_;_;_;_;_;_;_;_;"O";_;_;_;_;_;_;_;_; "O"] -> Some (["a7";"d7";"g7"]) 
          | [ "O";_;_;"O";_;_; "O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["a1";"b2";"c3"]) //Diagonal 
          | [ _;_; "O";_;_;"O";_;_;"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (["e3";"f2";"g1"])
          | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O";_;_] -> Some  (["a7";"b6";"c5"])
          | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O"] -> Some (["e5";"f6";"g7"])
          | _ -> None 
    | _ -> None 
          
//let b = [ "a1";"a4"; "a7";"b2";"b4";"b6"; "c3";"c4";"c5"; "d1";"d2";"d3";"d5";"d6";"d7"; "e3"; "e4"; "e5";"f2";"f4";"f6"; "g1"; "g4"; "g7"]
let shooting_occur list player mill_list =    //shooting cow function takes in the main list, the above some(mill list), and which player 
  printfn  "%A's turn.  Type the number with the cow you want to shoot." player  //read the user input on which cow they want to kill 
  let n = System.Console.Readline()
    let outputofshooting=
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
  //  the above part focuses on getting rid of the old main list and replacing the killed cow with the original coordinate position
  //  the below code disbands the mill formation
  let update_list =
     let rec disband_mill oldlist newlist millpositions =    //recursion that takes in the main list, newlist and the millpositions
         match oldlist with                                
          | [] -> List.rev newlist
          | x:: rest -> 
                 match x = "P" or "O", millpositions with      //check if the oldlist's element is P or O, meaning the beginning of the mill formation
                  | false, _::_ -> disband_mill rest x::nestlist millpositions  //not the beginning of the mill then you can add oldlist's element into the new list
                  | _, position:: leftover -> disband_mill rest position::nestlist leftover //the beginning of the mill, then you add its position back into the new list
     disband_mill list [] mill_list      





let adding_input (n: string) (list:string list) player () =      //takes the inputread, the list, and the player 
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
printfn  "%A's turn.  Type the number of the cell that you want to play into." player    //first player goes and places cow 
 let _n = System.Console.Readline()
   let updated_list= adding_input n b player      
     let rec counter list iteration player =  //ensures that each player got to put 12 cows down.
       match iteration with
        | 23 -> 0                     //base case that stops the recursion . It is 23 because of the top code, read updated list
        | _ ->
              let new_player = swapPlayer player
              printfn  "%A's turn.  Type the number of the cell that you want to play into." new_player
              let _n = System.Console.Readline()
              let updated_list= adding_input n b new_player
              counter updated_list iteration+1 new_player               
     counter updated_list 0 player  //pass the arguments of updated_list, iteration value starting at 0, and the player



[<EntryPoint>]
let main argv = 
  
  
    System.Console.ReadKey()|>ignore
    0 // return an integer exit code
