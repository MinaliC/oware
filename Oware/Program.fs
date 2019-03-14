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
    let a,b,c,d,e,f=(board)
    match n with 
         |1 | 7->a
         |2 | 8->b
         |3 | 9->c
         |4 | 10->d
         |5 | 11->e
         |6 | 12->f
         |_->failwith "Not implemented"
        
            
let useHouse n board=
    
    failwith "Not implemented"

//let runningGame 


let start position =
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
