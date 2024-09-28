open Nact

type msg = 
| BuildCasa
| TruckVisitPassBy
| TruckVisitWithStop
| BuildEvedamiaField
// | BuildStorage
// | BuildMoxaField
// | BuildMoxalinLab
// | BuildAirport
// | DestroyCasa
// | DestroyStorage
// | DestroyEvedamiaField
// | DestroyMoxaField
// | DestroyMoxalinLab
// | DestroyAirport

type cellId = CellId((int, int))

type cellFacility = 
  | Casa(actorRef<Casa.msg>)
  | EvedamiaField(actorRef<EvedamiaField.msg>)
  // | MoxaField(actorRef<moxaFieldMsg>)
  // | MoxalinLab(actorRef<moxalinLabMsg>)
  // | Airport(actorRef<airportMsg>)
  // | Storage(actorRef<storageMsg>)

type cellState = {
  id: cellId,
  // paramilitaryProbability: float,
  // paramilitaryQuantity: int,
  moxaProductivity: float,
  evedamiaProductivity: float,
  ownPlayer?: option<Nact.actorRef<Player.msg>>,
  facility?: option<cellFacility>,
  roadQuality: float,
}

type cell = Cell(cellState, actorRef<msg>)

let validate = (sender, owner) => sender === owner

let make = (game, cellInitState: cellState) => spawn(~name=String.make(cellInitState.id), game, async (state: cellState, (sender, msg), _) =>
  switch msg {
  | BuildCasa => state
  | BuildEvedamiaField => state
  | TruckVisitPassBy => state
  | TruckVisitWithStop => state
  
  // | BuildStorage => state
  // | BuildMoxaField => state
  // | BuildMoxalinLab => state
  // | BuildAirport => state
  },
  _ => { ...cellInitState, ownPlayer: None },
)