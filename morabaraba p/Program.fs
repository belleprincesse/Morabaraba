open System.Security.Cryptography
open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Player =
| P             //different colours
| O


type PlayerInfo =
   { Name : Player
     IntitalCows : int
     MillHas : (char*int) list
     Cows: (char*int) list
   }

 type Game = 
   { 
    p1: PlayerInfo
    p2: PlayerInfo
   }


  type GameResult =
  | Winner | Ongoing | Draw

let swapPlayer x =
    match x with
    | P -> O
    | O -> P
   

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


let millformation list player =
   match player with 
   | P ->
         match list with 
         | ["P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('a',1),('a',2),('a',4))//horizontal cases
         | [ _;_;_; "P";"P";"P"; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('b',2),('b',4),('b',6))
         | [ _;_; _;_;_;_; "P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('c',3),('c',4),('c',5))
         | [ _;_; _;_;_;_; _;_;_;"P";"P";"P";_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('d',1),('d',2),('d',3))
         | [ _;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_;_;_;_;_;_;_] -> Some (('d',5),('d',6),('d', 7))
         | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_;_;_;_]-> Some (('e',3), ('e',4), ('e',5))
         | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"P";"P";"P";_;_;_]-> Some (('f',2),('f',4),('f',6))
         | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; _;_;_; "P";"P";"P"] -> Some (('g',1),('g',4), ('g',7))
         | [ "P";_;_;_;_;_;_;_;_; "P";_;_;_;_;_;_;_;_;_;_;_;"P";_;_] -> Some (('a',1),('d',1),('g',1))   //vertical cases
         | [ _;_; _;"P";_;_;_;_;_;_;"P";_;_;_;_;_;_;_;"P";_;_; _;_;_] -> Some (('b',2),('d',2),('f',2))
         | [ _;_;_;_;_;_; "P";_;_;_;_;"P";_;_;_; "P";_;_;_;_;_;_;_;_] -> Some (('c',3),('d',3),('e',3))
         | [ _;"P";_;_;"P";_;_;"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('a',4),('b',4),('c',4))
         | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P";_] -> Some (('e',4),('f',4),('g',4))
         | [ _;_; _;_;_;_;_;_;"P";_;_;_;"P";_;_;_;_; "P";_;_;_;_;_;_] -> Some (('c',5),('d',5),('e',5))
         | [ _;_;_;_;_;"P";_;_;_;_;_;_;_;"P";_;_;_;_;_;_;"P";_;_;_] -> Some (('b',6),('d',6),('f',6))
         | [ _;_; "P";_;_;_;_;_;_;_;_;_;_;_;"P";_;_;_;_;_;_;_;_; "P"] -> Some (('a',7),('d',7),('g',7))
         | [ "P";_;_;"P";_;_; "P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]  -> Some (('a',1),('b',2),('c',3))  //Diagonal
         | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P";_;_] -> Some (('e',3),('f',2),('g',1))
         | [ _;_; "P";_;_;"P";_;_;"P";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]  -> Some (('a',7),('b',6),('c',5))
         | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "P";_;_;"P";_;_; "P"] -> Some (('e',5),('f',6),('g',7))
         | _ -> None
   | O ->
         match list with 
         | ["O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_]  -> Some (('a',1),('a',2),('a',4))   //horizontal cases
         | [ _;_;_; "O";"O";"O"; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some(('b',2),('b',4),('b',6))
         | [ _;_; _;_;_;_; "O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('c',3),('c',4),('c',5))
         | [ _;_; _;_;_;_; _;_;_;"O";"O";"O";_;_;_;_;_;_;_;_;_;_;_;_] -> Some(('d',1),('d',2),('d',3))
         | [ _;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_;_;_;_;_;_;_] -> Some (('d',5),('d',6),('d', 7))
         | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_;_;_;_] -> Some (('e',3), ('e',4), ('e',5))
         | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;"O";"O";"O";_;_;_] -> Some (('f',2),('f',4),('f',6))
         | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; _;_;_; "O";"O";"O"]  -> Some (('g',1),('g',4), ('g',7))
         | [ "O";_;_;_;_;_;_;_;_; "O";_;_;_;_;_;_;_;_;_;_;"O";_;_;_] -> Some (('a',1),('d',1),('g',1))//vertical cases
         | [ _;_; _;"O";_;_;_;_;_;_;"O";_;_;_;_;_;_;_;"O";_;_; _;_;_] -> Some (('b',2),('d',2),('f',2))
         | [ _;_;_;_;_;_; "O";_;_;_;_;"O";_;_;_; "O";_;_;_;_;_;_;_;_] -> Some (('c',3),('d',3),('e',3))
         | [ _;"O";_;_;"O";_;_;"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('a',4),('b',4),('c',4))
         | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O";_] -> Some  (('e',4),('f',4),('g',4))
         | [ _;_; _;_;_;_;_;_;"O";_;_;_;"O";_;_;_;_; "O";_;_;_;_;_;_] -> Some (('c',5),('d',5),('e',5))
         | [ _;_;_;_;_;"O";_;_;_;_;_;_;_;"O";_;_;_;_;_;_;"O";_;_;_] -> Some  (('b',6),('d',6),('f',6))
         | [ _;_; "O";_;_;_;_;_;_;_;_;_;_;_;"O";_;_;_;_;_;_;_;_; "O"] -> Some(('a',7),('d',7),('g',7))
         | [ "O";_;_;"O";_;_; "O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('a',1),('b',2),('c',3))  //Diagonal
         | [ _;_; "O";_;_;"O";_;_;"O";_;_;_;_;_;_;_;_;_;_;_;_;_;_;_] -> Some (('e',3),('f',2),('g',1))
         | [ _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O";_;_] -> Some  (('a',7),('b',6),('c',5))
         | [ _;_; _;_;_;_;_;_;_;_;_;_;_;_;_;_;_; "O";_;_;"O";_;_; "O"] -> Some (('e',5),('f',6),('g',7))
         | _ -> None 
    

let shootingcows position (playersdetails: Game) (playerkills: PlayerInfo) (playerkilled: PlayerInfo)  =
  let rec updatinglists place playerlists oldkilled killedlist =
          match playerkilled.Cows, playerkills.MillHas with
          | [], _::_::_::rest -> 
                     let c ={playerlists.playerkills with MillHas = rest};  
                     let b = {c; d}
                     b
          | x::rest, _ ->
                       match place=x with
                       | false -> updatinglists place playerlists rest::oldkilled x::killedlist 
                       | _     -> updatinglists place playerlists rest::oldkilled killedlist 
  updatinglists position playersdetails [] []



let checkandaddmills (inputplayer:PlayerInfo) inputlist =
   let rec addingmills (whoseplaying: PlayerInfo) previoustuple =
      let milltuples = millformation inputlist whoseplaying.Name
      match milltuples = previoustuple with 
      | true -> addingmills whoseplaying previoustuple
      | false -> 
               match milltuples with 
               | None -> whoseplaying
               | Some ((b,c),(d,e),(f,g)) -> 
                           match whoseplaying.MillHas with
                           | [] -> addingmills {whoseplaying with MillHas= (b,c)::(d,e)::(f,g)::[]} previoustuple
                           | x::rest -> addingmills {whoseplaying with MillHas= (b,c)::(d,e)::(f,g)::x::rest} previoustuple 
   addingmills inputplayer None
                    

let validposition place inputlist turn iteration =
   let rec ispositiontaken position mainlist whose_turn =
         match List.exists((=) position) mainlist with
         | false -> 
                match iteration with
                | 24 -> position
                | _  ->
                      printfn "%A choose another option, clearly it is taken or not part of the list." whose_turn
                      let place = System.Console.ReadLine()
                      let b = ispositiontaken place mainlist whose_turn 
                      b  
         | _ -> position
   ispositiontaken place inputlist turn
   
    
let whichplayerinput (whoseplaying:Game) position player_turn=
   match player_turn with 
    | P ->
         let c = {p1={whoseplaying.p1 with IntitalCows=whoseplaying.p2.IntitalCows+1; Cows =position::[]}; p2= {whoseplaying.p2 with IntitalCows=whoseplaying.p2.IntitalCows; Cows = whoseplaying.p2.Cows}}
         c 
    | O ->
         let c = {p1= {whoseplaying.p1 with IntitalCows=whoseplaying.p1.IntitalCows; Cows = whoseplaying.p1.Cows}; p2= {whoseplaying.p2 with IntitalCows= whoseplaying.p2.IntitalCows+1; Cows =position::[]}}
         c

let index (mylists:string list) (number:string) (player:Player) =
  let rec placement (mylist:string list) (letter:string)  (newlist:string list) (playername:Player)  =
     match mylist with 
     |[]-> List.rev newlist
     |x::rest->
         match x=letter with
         |true->
                match playername with 
                | P-> placement rest letter ("P"::newlist) playername
                | O-> placement rest letter ("O"::newlist) playername
                
               
         |_ -> placement rest letter (x::newlist) playername 
  placement mylists number [] player

let playing_initate input = 
    printfn "Enter any key to begin the game" //function puts in the position of the cows
    printboard input
    let p = {Name= P; IntitalCows= 0 ; MillHas= []; Cows =[]}
    let O = {Name= O; IntitalCows= 0 ; MillHas= []; Cows =[]}
    let playerdata = {p1=p;p2=O}
    System.Console.ReadKey()|>ignore
    let whoseturn = Player.P
    let rec counter iteration whose_turn inputlist gameplayers =
            
         printfn "%A's turn.  Type the number of the cell that you want to play into." whose_turn
         let position = validposition (System.Console.ReadLine()) inputlist whose_turn iteration
         let _, stringconversionint = System.Int32.TryParse ((sprintf "%c" position.[1]))
         let constrain = position.[0],stringconversionint
         match iteration, gameplayers.p1.IntitalCows with
         | 24, 12 -> 
                     inputlist
         | _, _   -> 
                 let output = index inputlist position whose_turn
                 let updateplayers = whichplayerinput gameplayers constrain whose_turn
                 Console.Clear()
                 printboard output
                 counter (iteration+1) (swapPlayer(whose_turn)) output updateplayers
    counter 0 whoseturn input playerdata

let millandmoving mainlist playerdetails =
     printfn "Lets check for Mills"
     let player1 = checkandaddmills playerdetails.p1 mainlist
     printfn "Player P, you have the following mills: %A " player1.MillHas
     let player2 = checkandaddmills playerdetails.p2 mainlist
     printfn "Player O, you have the following mills: %A " player2.MillHas
     let whosegoing =
       match player1.MillHas, player2.MillHas with 
       | [],[] -> printfn "Lets fly, choose another position for example position " 
                  let position = validposition (System.Console.ReadLine())
                  let _, stringconversionint = System.Int32.TryParse ((sprintf "%c" position.[1]))
                  let constrain = position.[0],stringconversionint
                  movingcows constrain 

       | [],_  -> printfn "Player O gets to kill a cow"
                  printfn "Which of Player's P cow do you want to get rid of "
                  let position = validposition (System.Console.ReadLine())
                  let _, stringconversionint = System.Int32.TryParse ((sprintf "%c" position.[1]))
                  let constrain = position.[0],stringconversionint
                  shootingcows constrain playerdetails.p1 playerdetails.p2
       | _ ,[] -> prinfn "Player P gets to kill a cow"
      




   

 




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
    
