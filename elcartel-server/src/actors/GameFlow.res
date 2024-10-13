open Glob
open Nact

let system = start()

type msg = 
| Start
| End


let initialMarketRates: Messages.marketRates = {
  evedamiaToLumeros: 10.0,  
  moxalinToLumeros: 1000.0,
}

type gameState = {
    cellsMap: array<array<Messages.cell>>,
    marketRates: Messages.marketRates,
    players: array<Messages.player>
}

let make = (players: array<playerId>, cellsMap: array<array<Cell.cellInitState>>) => 
  spawn(~name=`Game${Float.toString(Math.random())}`, system, async (state: gameState, msg:msg, ctx) =>
    switch msg {
    | Start => {
          state
        }
        | End => {
          stop(ctx.self)
          state
        }
    },
    ctx => { 
      cellsMap: cellsMap->Array.mapWithIndex((row, y) => 
      row->Array.mapWithIndex((cell, x) => 
        Messages.Cell({x, y}, Cell.make(ctx.self, {x, y}, cell)))),
      marketRates: initialMarketRates,
      players: players->Array.map(playerId => Messages.Player(playerId, Player.make(ctx.self, playerId))) 
    },
)
