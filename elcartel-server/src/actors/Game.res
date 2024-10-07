open Nact
open Glob

type msg = 
| Start({users: array<user>})
| End


let initialMarketRates: Messages.marketRates = {
  evedamiaToLumeros: 10.0,  
  moxalinToLumeros: 1000.0,
}

type gameState = {
    cellsMap: array<array<actorRef<Cell.msg>>>,
    players: array<Messages.player>
}

