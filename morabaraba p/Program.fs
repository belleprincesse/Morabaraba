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


let possiblemovepositions position inputlist =    //provides all the possible cow moves based on player's position
  let movelists = 
               match position with    
               | ('a',1) -> ["a4";"b2";"d1"]
               | ('a',4) ->["a7";"b4";"a1"]
               | ('a',7) ->[ "a4";"b6"; "d7"]
               | ('b',2) ->[ "a1";"b4"; "c3";"d2"]
               | ('b',4) ->[ "a4";"b6";"c4";"b2"]
               | ('b',6) ->[ "a7"; "d6";"c5";"b4"]
               | ('c',3) ->["c4"; "b2";"d3"] 
               | ('c',4) ->["c5";"b4";"d3"]
               | ('c',5) ->["b6";"d5";"c4"]
               | ('d',1) ->["a1";"d2"; "g1"]
               | ('d',2) ->["d1";"b2"; "d3"; "f2" ]
               | ('d',3) ->["c3";"e3";"d2"]
               | ('d',5) ->["c5"; "e5";"d6"]
               | ('d',6) ->[ "b6";"d7";"f6";"d5"]
               | ('d',7) ->["a7"; "g7";"d6"]
               | ('e',3) ->["d3";"e4";"f2"]
               | ('e',4) ->["e5"; "f4";"e3"]
               | ('e',5) ->["d5"; "e4";"f6"]
               | ('f',2) ->["e3";"f4";"d2";"g1" ]
               | ('f',4) ->["e4";"f6";"g4"; "f2"]
               | ('f',6) ->["d6";"g7";"e5";"f4"]
               | ('g',1) ->[ "d1";"f2"; "g4"]
               | ('g',4) ->["f4";"g7";"g1"]
               | ('g',7) ->["g4";"f6";"d7"]
               | _ -> []
  let rec checking possiblemoves newlist=
     match possiblemoves with
     | [] -> List.rev newlist
     | x::rest -> 
          match List.exists ((=)x) inputlist with
          | true -> checking rest (x::newlist)
          | _ -> checking rest newlist
  checking movelists []
     

let flying (whosemoving: PlayerInfo) positionto positionfrom =  //function that allows free movement across the board
 let updatedlist = 
    let rec changeoccurs playeroldlist playernewlist  =
        match playeroldlist  with
        | [] -> List.rev playernewlist
        | x::rest -> 
               match positionfrom=x with
               | true -> changeoccurs rest (positionto::playernewlist) 
               | _ -> changeoccurs rest (x::playernewlist)
    changeoccurs whosemoving.Cows []
 {whosemoving with Cows= updatedlist}  

let updateboardlist  (whosemoving: PlayerInfo) positionto positionfrom mainlist=    //changes the board because of flying or restricted positional moves
  let player1orplayer2 = 
      match whosemoving.Name with
      | P -> "P"
      | O -> "O"
  let rec changeboard workinglist checklist newlist =
        match workinglist, checklist with                
        | m::mainrest, i::initialrest ->
                  match positionfrom =i,positionto=m with      //a1,P,a7 a4 to a4  a1.a4.p
                  | true, false -> changeboard mainrest initialrest (positionfrom::newlist)
                  | false, true -> changeboard mainrest initialrest (player1orplayer2::newlist)
                  | _,_ -> changeboard mainrest initialrest (m::newlist)
        | _,_ ->  
                  printboard (List.rev newlist)
                  List.rev newlist   
  changeboard mainlist initiallist [] 

let updatingboard position mainlist=                                              //updates board after cow shoot
   let rec changingboard inputlist checklist newlist =
      match inputlist, checklist with
      | m:: mainrest, i::initialrest ->
                                 match position = i with 
                                 |  true -> changingboard mainrest initialrest (position::newlist)
                                 |  _  ->  changingboard mainrest initialrest (m::newlist)
      | _,_ ->
               printboard (List.rev newlist)         
               List.rev newlist 
   changingboard mainlist initiallist []                          
       

let milldisband (whoselostaMill: PlayerInfo)=               //gets rid of one mill after player kills a cow
      match whoselostaMill.MillHas with
      | _::_::_::rest -> {whoselostaMill with MillHas = rest} 
      | _ -> {whoselostaMill with MillHas = []} 

let shootingcows position (whosecowgotkilled: PlayerInfo) =  //shooting happens here and updates the victim's list
  let updated_list =
     let rec updatinglists position killedlist newlist=
        match killedlist with
        | [] -> List.rev newlist
        | (a,b)::rest -> 
              match position= (a,b) with
              | true -> updatinglists position [] (rest@newlist)
              | _ -> updatinglists position rest ((a,b)::newlist)
     updatinglists position whosecowgotkilled.Cows []
  {whosecowgotkilled with Cows= updated_list}   


let checkandaddmills (inputplayer:PlayerInfo) inputlist =    //checks whether each player has a mill or not
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
 
let validpositionandconversion position inputlist=       //this function checks the position exists on the board and converts into a char*int to prepare insert into PlayerInfo
  let conversionposition= 
    let rec findingvalidposition place =
       match  List.exists ((=) place) inputlist with
       | true -> place
       | _ -> 
              printfn "Try again, pick another valid cow position"
              let potentialposition = System.Console.ReadLine() 
              findingvalidposition potentialposition
    findingvalidposition position 
  let _, stringconversionint = System.Int32.TryParse ((sprintf "%c" conversionposition.[1]))
  let constrain = conversionposition.[0],stringconversionint 
  constrain

let validposition place inputlist turn iteration =    //checks the valid position for the placing the cows at the start of the game 
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
   
    
let whichplayerinput (whoseplaying:Game) position player_turn=  //updates each playerinfo when placing the cows happen
   match player_turn with 
    | P ->
         let c = {p1={whoseplaying.p1 with IntitalCows=whoseplaying.p2.IntitalCows+1; Cows =position::[]}; p2= {whoseplaying.p2 with IntitalCows=whoseplaying.p2.IntitalCows; Cows = whoseplaying.p2.Cows}}
         c 
    | O ->
         let c = {p1= {whoseplaying.p1 with IntitalCows=whoseplaying.p1.IntitalCows; Cows = whoseplaying.p1.Cows}; p2= {whoseplaying.p2 with IntitalCows= whoseplaying.p2.IntitalCows+1; Cows =position::[]}}
         c

let index (mylists:string list) (number:string) (player:Player) =    //controls placement
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
     

let mainmoving (playerdetails: Game) inputlist =                  //main moving function that calls all related moving or flying functions 
   printfn "Sorry, you lost a cow but you have a chance to move towards safety. Which cow's position do you want to move" 
   let positionfrom = validpositionandconversion (System.Console.ReadLine()) inputlist
   let convertedposition = possiblemovepositions  positionfrom 
   printfn "Which positions you can go: %A, where would you like to go?" convertedposition
   let positionto = validpositionandconversion (System.Console.ReadLine()) inputlist
   let updateplayer = flying playerdetails.p1 positionto positionfrom
   let newplayerdata = {p1= updateplayer; p2 = playerdetails.p2}
   let updated_boardlist = updateboardlist updateplayer (sprintf "%A" positionto) (sprintf "%A" positionfrom) inputlist
   newplayerdata updated_boardlist 
   

let mainmill inputlist playerdetails =                //main mill function that calls all related mill functions 
    printfn "Lets check if anyone has mills"
    let playermillupdate1 = checkandaddmills playerdetails.p1 inputlist
    printfn "PlayerP, you have the following mills: %A" playermillupdate1.MillHas 
    let playermillupdate2 = checkandaddmills playerdetails.p2 inputlist
    printfn " PlayerO, you have the following mills: %A" playermillupdate2.MillHas 
    let whogoesfirst, whoselosescow =
        match playermillupdate1.MillHas  with
        | [] -> playermillupdate2, playermillupdate1
        | _  ->  playermillupdate1, playermillupdate2   
    printfn "%A, goes first and select a cow to shoot. Please provide the position" whogoesfirst
    let position = System.Console.ReadLine() 
    let constrain = validpositionandconversion position inputlist
    let Playerwholostcow = shootingcows constrain whoselosescow
    let Playerwholosesmill = milldisband whogoesfirst
    let newplayerdata = {p1=Playerwholostcow;p2=Playerwholosesmill}
    let updatedlist = updatingboard position inputlist
newplayerdata updatedlist
         

let playing_initate input =                          //function that encompasses the start of the game, which places each cow
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
         | 24, 12 -> mainmill inputlist playerdata
                     
         | _, _   -> 
                 let output = index inputlist position whose_turn
                 let updateplayers = whichplayerinput gameplayers constrain whose_turn
                 Console.Clear()
                 printboard output
                 counter (iteration+1) (swapPlayer(whose_turn)) output updateplayers
    counter 0 whoseturn input playerdata





[<EntryPoint>]
let main argv = 
    
    playing_initate initiallist |> printboard
   
    System.Console.ReadKey()|>ignore
    0
    
