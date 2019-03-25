module Oware

type StartingPosition =
    | South
    | North

type GamePhases=
    |SouthTurn
    |NorthTurn
    |Draw
    |SouthWon
    |NorthWon 

type Player={
    house:(int*int*int*int*int*int)
    captureSeeds:int
    }

type Board=
    { player1:Player
      player2:Player
      phase:GamePhases
    }

let getCurrentPlayer gamePhase board =
   match gamePhase with
   |SouthTurn-> board.player1.house//"player1"
   |NorthTurn-> board.player2.house//"player2"
   |_ -> failwith "Not implemented"

let getSeeds n board =
    let a=
        match n with 
        |1|2|3|4|5|6 -> board.player1
        |7|8|9|10|11|12 -> board.player2
    let a,b,c,d,e,f=(a.house)
    match n with 
         |1 | 7->a
         |2 | 8->b
         |3 | 9->c
         |4 | 10->d
         |5 | 11->e
         |6 | 12->f
         |_->failwith "Not implemented"

     
let setSeeds n board = 
    (*let a,b,c,d,e,f = (board)
    match n with 
        |1 | 7->(a+1,b,c,d,e,f)
        |2 | 8->(a,b+1,c,d,e,f)
        |3 | 9->(a,b,c+1,d,e,f)
        |4 | 10->(a,b,c,d+1,e,f)
        |5 | 11->(a,b,c,d,e+1,f)
        |6 | 12->(a,b,c,d,e,f+1)
        |_->failwith "Not implemented"*)
    let (a,b,c,d,e,f) = board.player1.house
    let (g,h,i,j,k,l) = board.player2.house
    match n with
    |1  -> {board with player1 = {board.player1 with house = (a+1,b,c,d,e,f)}}
    |2  -> {board with player1 = {board.player1 with house = (a,b+1,c,d,e,f)}}
    |3  -> {board with player1 = {board.player1 with house = (a,b,c+1,d,e,f)}} 
    |4  -> {board with player1 = {board.player1 with house = (a,b,c,d+1,e,f)}}
    |5  -> {board with player1 = {board.player1 with house = (a,b,c,d,e+1,f)}} 
    |6  -> {board with player1 = {board.player1 with house = (a,b,c,d,e,f+1)}}
    |7  -> {board with player2 = {board.player2 with house = (g+1,h,i,j,k,l)}}
    |8  -> {board with player2 = {board.player2 with house = (g,h+1,i,j,k,l)}}
    |9  -> {board with player2 = {board.player2 with house = (g,h,i+1,j,k,l)}}
    |10 -> {board with player2 = {board.player2 with house = (g,h,i,j+1,k,l)}} 
    |11 -> {board with player2 = {board.player2 with house = (g,h,i,j,k+1,l)}}
    |12 -> {board with player2 = {board.player2 with house = (g,h,i,j,k,l+1)}}
    |_  -> failwith "Invalid house number."


let seedsZero n board =
    (*let a= 
    match n with 
        |1 | 7->(0,b,c,d,e,f)
        |2 | 8->(a,0,c,d,e,f)
        |3 | 9->(a,b,0,d,e,f)
        |4 | 10->(a,b,c,0,e,f)
        |5 | 11->(a,b,c,d,0,f)
        |6 | 12->(a,b,c,d,e,0)
        |_->failwith "Not implemented" *)
    //let board = a,b,c,d,e,f
    //board 
    let (a,b,c,d,e,f) = board.player1.house
    let (g,h,i,j,k,l) = board.player2.house
    match n with
    |1  -> {board with player1 = {board.player1 with house = (0,b,c,d,e,f)}}
    |2  -> {board with player1 = {board.player1 with house = (a,0,c,d,e,f)}}
    |3  -> {board with player1 = {board.player1 with house = (a,b,0,d,e,f)}} 
    |4  -> {board with player1 = {board.player1 with house = (a,b,c,0,e,f)}}
    |5  -> {board with player1 = {board.player1 with house = (a,b,c,d,0,f)}} 
    |6  -> {board with player1 = {board.player1 with house = (a,b,c,d,e,0)}}
    |7  -> {board with player2 = {board.player2 with house = (0,h,i,j,k,l)}}
    |8  -> {board with player2 = {board.player2 with house = (g,0,i,j,k,l)}}
    |9  -> {board with player2 = {board.player2 with house = (g,h,0,j,k,l)}}
    |10 -> {board with player2 = {board.player2 with house = (g,h,i,0,k,l)}} 
    |11 -> {board with player2 = {board.player2 with house = (g,h,i,j,0,l)}}
    |12 -> {board with player2 = {board.player2 with house = (g,h,i,j,k,0)}}
    |_  -> failwith "Invalid house number."


let useHouse n board=
   //collecting
   let currentplayer = getCurrentPlayer board.phase board
   let checkhouse n =
       match n with
           |1|2|3|4|5|6 -> SouthTurn
           |7|8|9|10|11|12 -> NorthTurn
   let housenumber = n
   let seedsInHouse=getSeeds n board
   match seedsInHouse with
     |0-> board
     |_->
         let zeroHouse = seedsZero n board
         let rec sowing hn numhouse numseeds b =
              match numseeds > 0 with
              |true -> 
                   match numhouse = hn with 
                   |true -> sowing hn (numhouse+1) (numseeds) b
                   |false -> match numhouse < 13 with
                             |true -> let newBoard = setSeeds numhouse b
                                      sowing hn (numhouse+1) (numseeds-1) newBoard 
                             |false -> sowing hn 1 (numseeds) b
              |false -> b 
         match seedsInHouse = 0 with
         |false -> sowing housenumber n seedsInHouse zeroHouse
         |true -> board

let start position =
   //failwith "Not implemented" 
    let p1={house=(4,4,4,4,4,4);captureSeeds=0}
    let p2={house=(4,4,4,4,4,4);captureSeeds=0}
    match position with 
        |South->{player1=p1;player2=p2;phase=SouthTurn}
        |North->{player1=p1;player2=p2;phase=NorthTurn}


let score board = 
    (board.player1.captureSeeds,board.player2.captureSeeds)


let gameState board =
    match board.phase with
    |SouthTurn->"South's turn"
    |NorthTurn->"North's turn"
    |Draw->"Game ended in a draw"
    |SouthWon->"South won"
    |NorthWon->"North won" 


[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
