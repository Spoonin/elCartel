open Nact

type msg = Types.cellMsg

type cell = actorRef<msg>

type cellInitState = {
  moxaProductivity: float,
  evedamiaProductivity: float,
}

type cellState = {
  ...cellInitState,
  ownPlayer: option<Types.player>,
  facility: option<Types.facility>,
}

let defaultPassTruTime = 10.0 *. Float.fromInt(second)

let idToString = (id: Types.cellId) => `${Int.toString(id.x)}x${Int.toString(id.y)}`
let toString = (Types.Cell(id, _)) => idToString(id)

let make = (game, id: Types.cellId, cellInitState: cellInitState) => spawn(~name=`x_${Int.toString(id.x)}y_${Int.toString(id.y)}`, game, async (state: cellState, msg, ctx) =>
  switch msg {
  | Types.InitialCasa(player) => {
    let Types.Player(_, ownPlayer) = player
    {
      ...state,
       ownPlayer: Some(player),
       facility: Some(Types.Casa(Casa.make(ownPlayer, ctx.name))),
    }
  }
  | Types.BuildCasa(player) => {
    switch state.facility {
      | Some(_) => {
        Js.log(`Cell ${ctx.name} is occupied - build at another cell`)
        state
      }
      | None => {
        switch state.ownPlayer {
          | Some(Player(ownerId, ownerActor)) => {
            if player == ownerId {
              let casa = Casa.make(ownerActor, ctx.name)
              casa->dispatch(Types.Build)

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
              ef->dispatch(Types.Build)

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
  | VehicleVisit(agent) => {
    switch state.facility {
    | Some(EvedamiaField(ef)) => agent->dispatch(Some(LoadResources(EvedamiaField(ef))))
    | Some(Casa(c)) => agent->dispatch(Some(UnloadResources(Casa(c))))
    | Some(Dealer(d)) => agent->dispatch(Some(LoadResources(Dealer(d))))
    | None => agent->dispatch(None)
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