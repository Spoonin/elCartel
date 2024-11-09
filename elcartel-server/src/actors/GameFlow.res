open Nact

let system = start()

type msg = 
| Start(array<(Types.playerId, Types.cellId)>)
| End


let initialMarketRates: Types.marketRates = {
  evedamiaToLumeros: 10.0,  
  moxalinToLumeros: 1000.0,
}

let firstNames = ["Alejandro", "Andrés", "Carlos", "Diego", "Eduardo", "Fernando", "Francisco", "Gabriel", "Gustavo", "Javier", "José", "Juan", "Luis", "Manuel", "Miguel", "Pablo", "Rafael", "Ricardo", "Santiago", "Sebastián"]
let lastNames = ["García", "Martínez", "González", "Pérez", "Rodríguez", "Sánchez", "Ramírez", "Torres", "Flores", "Castillo", "Vázquez", "Morales", "Ríos", "Jiménez", "Díaz", "Reyes", "Ortiz", "Mendoza", "Cruz", "Castro", "Ruiz", "Vega", "Gutiérrez", "Chávez", "Ramos", "Álvarez", "Aguilar", "Domínguez"]

let getRandomName = (names) => Option.getOr(names[Js.Math.random_int(0, names->Array.length - 1)], "")

let randomName = () => `${getRandomName(firstNames)} ${getRandomName(lastNames)} ${Int.toString(Js.Math.random_int(1, 99999999))}`

type gameState = {
    cellsMap: array<array<Types.cell>>,
    marketRates: Types.marketRates,
    players: array<Types.player>,
    sicarios: dict<Nact.actorRef<Types.sicarioMsg>>,
}

let getOrCreateSicario = (state, mbSicario: option<Nact.actorRef<Types.sicarioMsg>>, gameFlow) => 
  switch mbSicario {
    | Some(sicario) => sicario
    | None => {
      let name = randomName()
      let sicario = Sicario.make(name, gameFlow)

      state.sicarios->Js.Dict.set(name, sicario)
      sicario
    }
  }

let make = (cellsMap: array<array<Cell.cellInitState>>, gamePlayers: dict<actorRef<Types.playerMsg>>) => 
  spawn(~name=`Game${Float.toString(Math.random())}`, system, async (state: gameState, msg:msg, ctx) =>
    switch msg {
    | Start(players) => {
      let playersDictData: array<(string, Types.player, Types.cellId)> = players->Array.map(((PlayerId(pid), originCell)) => (pid, Types.Player(PlayerId(pid), Player.make(ctx.self, PlayerId(pid))), originCell))
      Array.forEach(playersDictData, ((key, value, originCell)) => {
        Array.push(state.players, value)
        let Types.Player(_, actor) = value
        gamePlayers->Js.Dict.set(key, actor)
        Option.forEach(state.cellsMap->Array.get(originCell.y), (row) => 
          Option.forEach(row->Array.get(originCell.x), (Types.Cell(_, cellActor)) => {
            cellActor->dispatch(Types.InitialCasa(value))
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
        Types.Cell({x, y}, Cell.make(ctx.self, {x, y}, cell)))),
      marketRates: initialMarketRates,
      players: [],
      sicarios: Js.Dict.empty(),
    },
)
