open Nact
open Glob

type msg = 
| BuildCasa
| TruckVisitPassBy(reply<float>)
| TruckVisitWithStop(reply<float>)
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

type cell = actorRef<msg>

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

let defaultPassByTime = 10.0 *. Float.fromInt(second)

let validate = (sender, owner) => sender === owner

let make = (game, cellInitState: cellState) => spawn(~name=String.make(cellInitState.id), game, async (state: cellState, msg, _) =>
  switch msg {
  | BuildCasa => state
  | BuildEvedamiaField => state
  | TruckVisitPassBy(Reply(cb)) => {
    cb(defaultPassByTime /. state.roadQuality)
    state
  }
  | TruckVisitWithStop(Reply(cb)) => {
    switch state.facility {
    | Some(Casa(casa)) => casa->dispatch(Casa.Build)
    | Some(EvedamiaField(evedamiaField)) => evedamiaField->dispatch(EvedamiaField.Build)
  }
  
  // | BuildStorage => state
  // | BuildMoxaField => state
  // | BuildMoxalinLab => state
  // | BuildAirport => state
  },
  _ => { ...cellInitState, ownPlayer: None },
)