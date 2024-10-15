open Glob
type t = {
    gameFlow: Nact.actorRef<GameFlow.msg>,
    cells: array<array<Cell.cellInitState>>,
    playersCandidates: Set.t<playerId>,
    players: dict<Nact.actorRef<Messages.playerMsg>>,
}

let getCell: (float, float) => Cell.cellInitState = (m, e) => { moxaProductivity: m, evedamiaProductivity: e, }
let initCells = [
        [getCell(0.2, 0.6), getCell(0.4, 0.8), getCell(0.6, 0.2), getCell(0.8, 0.4), getCell(0.1, 0.9)],
        [getCell(0.5, 0.9), getCell(0.7, 0.7), getCell(0.9, 0.5), getCell(0.1, 0.3), getCell(0.3, 0.1)],
        [getCell(0.8, 0.9), getCell(0.6, 0.7), getCell(0.4, 0.5), getCell(0.2, 0.3), getCell(0.9, 0.1)],
        [getCell(0.9, 0.8), getCell(0.7, 0.6), getCell(0.5, 0.4), getCell(0.3, 0.2), getCell(0.1, 0.9)],
        [getCell(0.1, 0.6), getCell(0.3, 0.8), getCell(0.5, 0.2), getCell(0.7, 0.4), getCell(0.9, 0.9)],
    ]

let players = Js.Dict.empty()

let gameState = {
    gameFlow: GameFlow.make(initCells, players),
    cells: initCells,
    playersCandidates: Set.make(),
    players,
}

let playerOriginCellsStack = [  {x: 0, y:4}, {x: 1, y:3}, {x: 3, y:3}, {x: 2, y:0}, {x: 4, y:2} ]

let hasStarted: t => bool = (game) => game.players != Js.Dict.empty()
let hasCandidates: t => bool = (game) => Set.size(game.playersCandidates) > 1
let startGame: t => unit = (game) => {
    let playersIds = game.playersCandidates
        ->Set.values
        ->Array.fromIterator

    let playersWithOrigins = playersIds->Array.map((pid) => (pid, Option.getExn(playerOriginCellsStack->Array.pop, ~message="Not enough player origins")))

    game.gameFlow->Nact.dispatch(
        GameFlow.Start(playersWithOrigins)
    )
}

let endGame: t => unit = (game) => {
    game.gameFlow->Nact.dispatch(GameFlow.End)
}