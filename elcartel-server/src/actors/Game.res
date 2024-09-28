open Nact
open Glob

type msg = 
| Start({users: array<user>})
| End

type gameState = {
    cellsMap: array<array<Cell.cell>>,
    players: array<actorRef<Player.msg>>
}

