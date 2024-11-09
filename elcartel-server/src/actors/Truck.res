open Nact
open Types

type truckId = TruckId(string)

let lumerosCapacity = 1000000
let evedamiaCapacity = 1000
let moxalinCapacity = 1000

type resource = 
    | LumerosPack(lumeros)
    | EvedamiaPack(evedamia)
    | MoxalinPack(moxalin)

type movement = Movement(cellId, cellId) // TODO: add speed

type truck = {
    id: truckId,
    load: option<resource>,
    position: Types.cell,
    route: option<array<Types.cell>>,
    movement: option<movement>,
    isLoading: bool,
}

let defaultSpeedCellsPerSec = 1.0 /. Float.fromInt(second)

let handleVisitResponse = async (self, state, stopReason) => {
  switch stopReason {
  | Some(LoadResources(facility)) => {
      switch facility {                
      | EvedamiaField(evedamiaField) => {
          evedamiaField->dispatch(Types.TruckCanLoad(evedamiaCapacity, self))
      }
      | Casa(_) => () // Other facilities cannot load resources
      | Dealer(_) => ()
      }

      state
  }
  | Some(UnloadResources(facility)) => {
      switch facility {
      | Casa(casa) =>
          switch state.load {
          | Some(resource) => 
              switch resource {
              | LumerosPack(amt) => casa->dispatch(ReceiveLumeros(amt))
              | _ => () // Nothing else can casa receive
              }
          | None => () // Nothing to unload
          }
      | EvedamiaField(_) => () // EvedamiaField cannot unload resources
      | Dealer(_) => () // Dealer cannot unload resources
      }
      state
  }
  | Some(SellResources(facility)) => {
      switch facility {
        | Dealer(dealer) => {
          switch state.load {
            | Some(resource) =>
              switch resource {
                | LumerosPack(_) => state // Lumeros cannot be sold
                | EvedamiaPack(evedamia) => {
                  let lumeros = await dealer->Glob.query100((agent) => #ExchangeEvedamia(evedamia, agent))
                  {
                    ...state,
                    load: Some(LumerosPack(lumeros)),
                  }
                }
                | MoxalinPack(moxalin) => {
                  let lumeros = await dealer->Glob.query100((agent) => #ExchangeMoxalin(moxalin, agent))
                  {
                    ...state,
                    load: Some(LumerosPack(lumeros)),
                  }
                }
              }
            | None => state // Nothing to sell
          }
        }
        | _ => state
      }
  }
  | None => state // Nothing to sell
  }
}


let getCell = (route: array<Types.cell>, cellId) => route->Array.find((Cell(id, _)) => id == cellId)

let isNeibour = (cellId1: cellId, cellId2: cellId) => {
    let dx = abs(cellId1.x - cellId2.x)
    let dy = abs(cellId1.y - cellId2.y)
    dx + dy <= 1
}

let getNextCell = (route: array<Types.cell>, currentCellId) => {
    let curStep = route->Array.findIndexOpt((Cell(cellId, _)) => currentCellId == cellId)
    
    switch curStep {
    | Some(step) => {
        let nextStep = step === Array.length(route) - 1 ? 0 : step + 1
        route[nextStep]
    }
    | None => None
  }
}

let getCellId = (cell: Types.cell) => {
  let Types.Cell(id, _) = cell
  id
}

let make = (patron, id, currentCell) => spawn(~name=String.make(id), patron, async (state: truck, msg, ctx) =>
    switch msg {
    | #StartRoute(route, circulate) => {
        let curStep = Option.getOr(
          route->Array.findIndexOpt((cell) => cell === state.position),
          0,
        )
    
        let Types.Cell(startId, startCellActor) = Option.getExn(route[curStep], ~message="Invalid route")

        let Types.Cell(nextId,_) = Option.getExn(getNextCell(route, startId), ~message="Invalid route")

        (await startCellActor->Glob.query100((actor) => VehicleVisit(actor)))->ignore

        if curStep < Array.length(route) - 1 || circulate {
          ctx.self->dispatch(#DriveTo(nextId))
        }

        {
          ...state,
          position: Types.Cell(startId, startCellActor),
          movement: Some(Movement(startId, nextId)),
          route: Some(route),
        }
      }
    | #SwitchCellTo(toCellId) => {
      let Types.Cell(curId,_) = state.position
    
      if(toCellId == curId) {
          if(isNeibour(curId, toCellId)) {
            let route = Option.getExn(state.route, ~message="No route set")
            let cell = getCell(route, toCellId)
              
            switch cell {
              | Some(Cell(cellId, cellActor)) => {
                let reason = await cellActor->Glob.query100((agent) => Types.VehicleVisit(agent))

                let updState = await handleVisitResponse(ctx.self, state, reason)

                Option.map(
                  getNextCell(route, cellId), 
                  (cell) => ctx.self->dispatch(#DriveTo(getCellId(cell)))
                )->ignore // TODO: double check this
                Js.log(`${ctx.name} - Appeared at ${Cell.idToString(cellId)}`)
                {
                  ...updState,
                  position: Cell(cellId, cellActor),
                }
              }
              | None => failwith("Cell is not in the current route")
            }
          } else {
            failwith("Cannot jump to a non-neighbour cell")
          }
      } else {
        state
      }       
    }
    | #DriveTo(toCellId) => {
      let Types.Cell(fromCellId,_) = state.position
      Glob.timeout(()=>ctx.self->dispatch(#SwitchCellTo(toCellId)), defaultSpeedCellsPerSec)

      Js.log(`${ctx.name} - Driving from ${Cell.idToString(fromCellId)} to ${Cell.idToString(toCellId)}`)
      
      {
        ...state,
        movement: Some(Movement(fromCellId, toCellId)),
      }
    }
    | #ReceiveLumeros(lumeros) => {
        ...state,
        load: Some(LumerosPack(lumeros)),
      }
    | #ReceiveEvedamia(evedamia) => {
        ...state,
        load: Some(EvedamiaPack(evedamia)),
      }
    | #ReceiveMoxalin(moxalin) => {
        ...state,
        load: Some(MoxalinPack(moxalin)),
      }
    }, 
    _ => {
        id,
        load: None,
        position: currentCell,
        route: None,
        movement: None,
        isLoading: false,
    }
)