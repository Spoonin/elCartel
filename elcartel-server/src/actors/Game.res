open Nact
open Glob

type msg = 
| Start({users: array<user>})
| End

type gameState = {
    cellsMap: array<array<actorRef<Cell.msg>>>,
    players: array<actorRef<Player.msg>>
}

