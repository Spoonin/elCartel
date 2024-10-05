open Nact
open Glob

type msg = Messages.cellMsg

type cell = actorRef<msg>

type cellState = {
  moxaProductivity: float,
  evedamiaProductivity: float,
  ownPlayer: option<Nact.actorRef<Player.msg>>,
  facility: option<Messages.facility>,
  roadQuality: float,
}

let defaultPassTruTime = 10.0 *. Float.fromInt(second)

let validate = (sender, owner) => sender === owner

let passThruTime = (state) => defaultPassTruTime /. state.roadQuality

let make = (game, id: Messages.cellId, cellInitState: cellState) => spawn(~name=`x#${Int.toString(id.x)}::y#${Int.toString(id.y)}`, game, async (state: cellState, msg, _) =>
  switch msg {
  | Messages.BuildCasa => state
  | BuildEvedamiaField => state
  | VehicleVisit(Reply(cb)) => {
    switch state.facility {
    | Some(EvedamiaField(ef)) => cb(Some(LoadResources(EvedamiaField(ef))))
    | Some(Casa(c)) => cb(Some(UnloadResources(Casa(c))))
    | None =>cb(None)
    }
    state
  }
  },
  _ => { ...cellInitState, ownPlayer: None },
)