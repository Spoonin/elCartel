open Nact
open Glob

type msg = Messages.cellMsg

type cell = actorRef<msg>

type cellInitState = {
  moxaProductivity: float,
  evedamiaProductivity: float,
}

type cellState = {
  ...cellInitState,
  ownPlayer: option<Messages.player>,
  facility: option<Messages.facility>,
}

let defaultPassTruTime = 10.0 *. Float.fromInt(second)

let idToString = (id: Messages.cellId) => `x#${Int.toString(id.x)}::y#${Int.toString(id.y)}`
let toString = (Messages.Cell(id, _)) => idToString(id)

let make = (game, id: Messages.cellId, cellInitState: cellInitState) => spawn(~name=`x#${Int.toString(id.x)}::y#${Int.toString(id.y)}`, game, async (state: cellState, msg, ctx) =>
  switch msg {
  | Messages.BuildCasa(player) => {
    switch state.facility {
      | Some(_) => {
        Js.log(`Cell ${ctx.name} is occupied - build at another cell`)
        state
      }
      | None => {
        switch state.ownPlayer {
          | Some(Player(ownerId, ownerActor)) => {
            if player === ownerId {
              let casa = Casa.make(ownerActor, ctx.name)
              casa->dispatch(Messages.Build)

              {
                ...state,
                facility: Some(Casa(casa)),
              }
            } else {
              Js.log(`Cell ${ctx.name} is not owned by another player - cannot build a casa`)
              state
            }
          }
          | None => {
            Js.log(`Cell ${ctx.name} has no owner - cannot build a casa`)
            state
          }
        }
      }
    }
  }
  | BuildEvedamiaField(player) => {
    switch state.facility {
      | Some(_) => {
        Js.log(`Cell ${ctx.name} is occupied - build at another cell`)
        state
      }
      | None => {
        switch state.ownPlayer {
          | Some(Player(ownerId, ownerActor)) => {
            if player === ownerId {
              let ef = EvedamiaField.make(ctx.self,ownerActor, state.evedamiaProductivity)
              ef->dispatch(Messages.Build)

              {
                ...state,
                facility: Some(EvedamiaField(ef)),
              }
            } else {
              Js.log(`Cell ${ctx.name} is not owned by another player - cannot build a casa`)
              state
            }
          }
          | None => {
            Js.log(`Cell ${ctx.name} has no owner - cannot build a casa`)
            state
          }
        }
      }
    }
  }
  | VehicleVisit(Reply(cb)) => {
    switch state.facility {
    | Some(EvedamiaField(ef)) => cb(Some(LoadResources(EvedamiaField(ef))))
    | Some(Casa(c)) => cb(Some(UnloadResources(Casa(c))))
    | Some(Dealer(d)) => cb(Some(LoadResources(Dealer(d))))
    | None =>cb(None)
    }
    state
  }
  },
  _ => {
      ownPlayer: None,
      facility: None,
      moxaProductivity: cellInitState.moxaProductivity,
      evedamiaProductivity: cellInitState.evedamiaProductivity,
  },
)