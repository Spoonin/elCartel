open Glob
type t = {
    cells: array<array<Cell.cellInitState>>,
    playersCandidates: Set.t<playerId>,
    players: dict<Nact.actorRef<Messages.playerMsg>>,
}

let getCell: (float, float) => Cell.cellInitState = (m, e) => { moxaProductivity: m, evedamiaProductivity: e, }

let gameState = {
    cells: [
        [getCell(0.2, 0.6), getCell(0.4, 0.8), getCell(0.6, 0.2), getCell(0.8, 0.4), getCell(0.1, 0.9), getCell(0.3, 0.7), getCell(0.5, 0.5), getCell(0.7, 0.3), getCell(0.9, 0.1)],
        [getCell(0.5, 0.9), getCell(0.7, 0.7), getCell(0.9, 0.5), getCell(0.1, 0.3), getCell(0.3, 0.1), getCell(0.5, 0.7), getCell(0.7, 0.5), getCell(0.9, 0.3), getCell(0.1, 0.1)],
        [getCell(0.8, 0.9), getCell(0.6, 0.7), getCell(0.4, 0.5), getCell(0.2, 0.3), getCell(0.9, 0.1), getCell(0.7, 0.3), getCell(0.5, 0.5), getCell(0.3, 0.7), getCell(0.1, 0.9)],
        [getCell(0.9, 0.8), getCell(0.7, 0.6), getCell(0.5, 0.4), getCell(0.3, 0.2), getCell(0.1, 0.9), getCell(0.3, 0.5), getCell(0.5, 0.7), getCell(0.7, 0.9), getCell(0.9, 0.1)],
        [getCell(0.1, 0.6), getCell(0.3, 0.8), getCell(0.5, 0.2), getCell(0.7, 0.4), getCell(0.9, 0.9), getCell(0.7, 0.7), getCell(0.5, 0.5), getCell(0.3, 0.3), getCell(0.1, 0.1)],
    ],
    playersCandidates: Set.make(),
    players: Js.Dict.empty(),
}

let hasStarted: t => bool = (game) => game.players == Js.Dict.empty()