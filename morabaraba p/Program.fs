open System.Security.Cryptography
open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Player =
| P             //different colours
| O
| Blank

type Player_info =
   { Name : Player
     placement_list : string list
     Mill : string list
     Cow_numbers: int}

let swapPlayer x =
    match x with
    | P -> O
    | O -> P
    | Blank -> failwith "no player was chosen"


(*let information ()=
  let player_p = {Name= Player.P; placement_list= []; Mill=[]; Cow_numbers=0}
  let player_O = {Name= Player.O; placement_list= []; Mill=[]; Cow_numbers=0}*)
  

let initiallist = [ "a1";"a4"; "a7";"b2";"b4";"b6"; "c3";"c4";"c5"; "d1";"d2";"d3";"d5";"d6";"d7"; "e3"; "e4"; "e5";"f2";"f4";"f6"; "g1"; "g4"; "g7"] 


let printboard (b:string list) =     //passing the list in
  printfn "  1   2   3      4    5    6   7   
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


(*let Mill_Formation list player =   // checks if a mill formation occured
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
    | _ -> None*)

let index (mylists:string list) (number:string) (player:Player) =
  let rec placement (mylist:string list) (letter:string)  (newlist:string list) (playername:Player)  =
     match mylist with 
     |[]-> List.rev newlist
     |x::rest->
         match x=letter with
         |true->
                match playername with 
                |P-> placement rest letter ("P"::newlist) playername
                |O-> placement rest letter ("O"::newlist) playername
                |_->failwith "mmmh whats this"
               
         |_ -> placement rest letter (x::newlist) playername 
  placement mylists number [] player

let playing_initate input = 
    printfn "enter any key to begin the game"  //function puts in the position of the cows
    printboard input
    System.Console.ReadKey()|>ignore
    let whoseturn = Player.P
    let rec counter iteration whose_turn inputlist =
        
         printfn "%A's turn.  Type the number of the cell that you want to play into." whose_turn
         let position = System.Console.ReadLine()
         match iteration with
         | 24 -> inputlist
         | _  -> let output = index inputlist position whose_turn
                 printboard output
                 counter (iteration+1) (swapPlayer(whose_turn)) output
    counter 0 whoseturn input


 




(*let Mainresult input =
  match input.p1.InitialCows,input.p2.InitialCows with
  |0,0->Winner
  |12,12->Ongoing

let WhoseTurn input=
    match input.p1.InitialCows>0 with 
    |true->placetoplace input|>(fun choosemove -> (printfn "Choose a position to place the cow from these %A",choosemove))
           
          //checks the placement 
    |false-> //checks the cow for avaliabble places to move

let placetoplace input=
    let checklist= [input.p1.Cows@input.p2.Cows] 
    let freespace free=List.filter((=) free )initiallist    
    freespace checklist//function that checks the main list with the occupied spaces and returns a list with uncccupied places

let usedSpace input 
    [input.p1.Cows@input.p2.Cows]|>List.map initiallist checkSpace  

let checkposition listlist
    //function that compares lists
    List.filter
    


let rec runGame newgame =
    let rec playAgain () =
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame newgame 
        |"N"|"n"    -> printfn "goodbye"
        | _ -> playAgain ()
    let rec continueGame game =
        match Mainresult game with 
        |Winner ->playAgain()
        |Draw-> playAgain ()
        |Ongoing->game |>SwapPlayer|>continueGame
    continueGame newgame
    let start ()=
       printf "which name do you want to use?"
       let player1={Name=System.Console.RealLine();Cows=[];InitialCows=12;millHas=[]}
       printf "which name do you want to use?"
       let player2={Name=System.Console.RealLine();Cows=[];InitialCows=12;millHas=[]}
       {p1=player1;p2=player2}

//take in the initiallist replace with the p for 1st play and redraw the board
  
let position initiallist =
   printfn "where do you want to place your cow %A" initiallist
   b=System.Console.ReadLine()*)
    
 
     

    




[<EntryPoint>]
let main argv = 
    
    playing_initate initiallist |> printboard
   
    System.Console.ReadKey()|>ignore
    (*printboard initiallist
    printfn "Please choose a letter between [P\O]"
    let playername = System.Console.ReadLine()
    printfn "Choose a position in one of these %A" initiallist
    let b=System.Console.ReadLine()
    let newlist=index initiallist b playername 
    printfn "you are player O"
    let playername = System.Console.ReadLine()
    printfn "Choose a position in one of these except the p positions %A" newlist
    printboard*)

    //System.Console.ReadKey()|>ignore
    0 // return an integer exit code
   


(*

let Mainresult input =
  match input.p1.InitialCows,input.p2.InitialCows with
  |0,0->
  |12,12->Ongoing

let WhoseTurn input=
    match input.p1.InitialCows>0 with 
    |true->placetoplace input|>(fun choosemove -> (printfn "Choose a position to place the cow from these %A",choosemove))
           
          //checks the placement 
    |false-> //checks the cow for avaliabble places to move

let placetoplace input=
    let free=usedSpace input
    let //function that checks the main list with the occupied spaces 

let usedSpace input 
    (input.p1.Cows@input.p2.Cows)|>List.map initiallist 

let checkposition listlist
    
    


let rec runGame newgame =
    let rec playAgain () =
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame newgame 
        |"N"|"n"    -> printfn "goodbye"
        | _ -> playAgain ()
    let rec continueGame game =
        match Mainresult game with 
        |Winner ->playAgain()
        |Draw-> playAgain ()
        |Ongoing->game|>WhoseTurn|>Millrecord|>swapPlayer|>continueGame
    continueGame newgame
    let start ()=
       printf "which name do you want to use?"
       let player1={Name=System.Console.RealLine();Cows=[];InitialCows=12;millHas=[]}
       printf "which name do you want to use?"
       let player2={Name=System.Console.RealLine();Cows=[];InitialCows=12;millHas=[]}
       {p1=player1;p2=player2}*)
    
