open Nact
open Glob
open Messages 

type msg = cellMsg

type cell = actorRef<msg>

type cellId = CellId((int, int))

type cellState = {
  id: cellId,
  moxaProductivity: float,
  evedamiaProductivity: float,
  ownPlayer: option<Nact.actorRef<Player.msg>>,
  facility: option<facility>,
  roadQuality: float,
}

let defaultPassTruTime = 10.0 *. Float.fromInt(second)

let validate = (sender, owner) => sender === owner

let passThruTime = (state) => defaultPassTruTime /. state.roadQuality

let make = (game, cellInitState: cellState) => spawn(~name=String.make(cellInitState.id), game, async (state: cellState, msg, _) =>
  switch msg {
  | BuildCasa => state
  | BuildEvedamiaField => state
  | VehicleVisitPassThru(Reply(cb)) => {
    cb(state->passThruTime)
    state
  }
  | VehicleVisitWithStop(Reply(cb)) => {
    switch state.facility {
    | Some(EvedamiaField(evedamiaField)) => cb({
        passThruTime: state->passThruTime,
        reason: LoadResources(EvedamiaField(evedamiaField)),
        stopTime: EvedamiaField.stopTime,
      })
    | None =>
      cb({
        passThruTime: state->passThruTime,
        reason: NoFacility,
        stopTime: 0.0,
      })
    | Some(Casa(casa)) => cb({
        passThruTime: state->passThruTime,
        reason: UnloadResources(Casa(casa)),
        stopTime: Casa.unloadStopTime,
      })
    }
    state
  }
  },
  _ => { ...cellInitState, ownPlayer: None },
)