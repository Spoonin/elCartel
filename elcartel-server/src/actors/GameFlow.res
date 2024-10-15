open Glob
open Nact

let system = start()

type msg = 
| Start(array<(playerId, cellId)>)
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

let make = (cellsMap: array<array<Cell.cellInitState>>, gamePlayers: dict<actorRef<Messages.playerMsg>>) => 
  spawn(~name=`Game${Float.toString(Math.random())}`, system, async (state: gameState, msg:msg, ctx) =>
    switch msg {
    | Start(players) => {
          let playersDictData: array<(string, Messages.player, cellId)> = players->Array.map(((PlayerId(pid), originCell)) => (pid, Messages.Player(PlayerId(pid), Player.make(ctx.self, PlayerId(pid))), originCell))
          Array.forEach(playersDictData, ((key, value, originCell)) => {
            Array.push(state.players, value)
            let Messages.Player(_, actor) = value
            gamePlayers->Js.Dict.set(key, actor)
            Option.forEach(state.cellsMap->Array.get(originCell.y), (row) => 
              Option.forEach(row->Array.get(originCell.x), (Messages.Cell(_, cellActor)) => {
                cellActor->dispatch(Messages.InitialCasa(value))
              }))
          })
          state
        }
        | End => {
          stop(ctx.self)
          stop(system)
          state
        }
    },
    ctx => { 
      cellsMap: cellsMap->Array.mapWithIndex((row, y) => 
      row->Array.mapWithIndex((cell, x) => 
        Messages.Cell({x, y}, Cell.make(ctx.self, {x, y}, cell)))),
      marketRates: initialMarketRates,
      players: []
    },
)
