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
    let a,b,c,d,e,f = (board)
    match n with 
        |1 | 7->(a+1,b,c,d,e,f)
        |2 | 8->(a,b+1,c,d,e,f)
        |3 | 9->(a,b,c+1,d,e,f)
        |4 | 10->(a,b,c,d+1,e,f)
        |5 | 11->(a,b,c,d,e+1,f)
        |6 | 12->(a,b,c,d,e,f+1)
        |_->failwith "Not implemented"

let seedsZero n board =
    let a,b,c,d,e,f = (board)
    match n with 
        |1 | 7->(0,b,c,d,e,f)
        |2 | 8->(a,0,c,d,e,f)
        |3 | 9->(a,b,0,d,e,f)
        |4 | 10->(a,b,c,0,e,f)
        |5 | 11->(a,b,c,d,0,f)
        |6 | 12->(a,b,c,d,e,0)
        |_->failwith "Not implemented"



let useHouse n board=
   failwith "Not implemented" 
  (*  let seedNum = getSeeds n board 
    let newBoard = seedsZero n board
    let rec seedMoves  hn v nb=
        match v >0 with
        | true ->let a = getSeeds (hn+1) nb
                 let b = setSeeds (hn+1) nb
                 seedMoves (hn+1) (v-1) b
        | false -> nb 
    match n< 13 && n> 0 with
        | true -> seedMoves n seedNum newBoard
        |false-> failwith " "
 *)
  

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

