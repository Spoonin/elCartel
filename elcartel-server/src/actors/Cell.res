open Nact
open Glob
open Casa

type msg = 
| BuildCasa
| BuildStorage
| BuildEvedamiaField
| BuildMoxaField
| BuildMoxalinLab
| BuildAirport
// | DestroyCasa
// | DestroyStorage
// | DestroyEvedamiaField
// | DestroyMoxaField
// | DestroyMoxalinLab
// | DestroyAirport

let cellSize = 10

type cellId = CellId(string)


type cellState = {
  id: cellId,
  origin: position,
  roadSpeedKoef: float,
  paramilitaryProbability: float,
  paramilitaryQuantity: int,
  moxaProductivity: float,
  evedamiaProductivity: float,
  ownPlayer?: option<Nact.actorRef<Player.msg>>,
}

type cellFacilities = {
  casa?: actorRef<casaMsg>,
  // storage?: actorRef<storageMsg>,
  // evedamiaField?: actorRef<evedamiaFieldMsg>,
  // moxaField?: actorRef<moxaFieldMsg>,
  // moxalinLab?: actorRef<moxalinLabMsg>,
  // airport?: actorRef<airportMsg>,
}

let validate = (sender, owner) => sender === owner

let make = (game, cellInitState: cellState) => spawn(~name=String.make(cellInitState.id), game, async (state: cellState, (sender, msg), _) =>
  switch msg {
  | BuildCasa => state
  | BuildStorage => state
  | BuildEvedamiaField => state
  | BuildMoxaField => state
  | BuildMoxalinLab => state
  | BuildAirport => state
  },
  _ => { ...cellInitState, ownPlayer: None },
)