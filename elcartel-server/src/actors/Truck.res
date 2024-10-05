open Nact
open Glob

type truckId = TruckId(string)

let lumerosCapacity = 1000000
let evedamiaCapacity = 1000
let moxalinCapacity = 1000

type resource = 
    | Lumeros(lumeros)
    | Evedamia(evedamia)
    | Moxalin(moxalin)

type movement = Movement(Messages.cellId, Messages.cellId) // TODO: add speed

type truck = {
    id: truckId,
    load: option<resource>,
    position: Messages.cell,
    route: option<array<Messages.cell>>,
    movement: option<movement>,
    isLoading: bool,
}

let defaultSpeedCellsPerSec = 1.0 /. Float.fromInt(second)

let handleVisitReply = (self, state) => {
    (stopReason: option<Messages.reason>) => {
        switch stopReason {
        | Some(LoadResources(facility)) =>
            switch facility {                
            | EvedamiaField(evedamiaField) => {
                evedamiaField->dispatch(Messages.TruckCanLoad(evedamiaCapacity, self))
            }
            | _ => () // TODO: other facilities
            }
        | Some(UnloadResources(facility)) => 
            switch facility {
            | Casa(_) =>
                switch state.load {
                | Some(resource) => 
                    switch resource {
                    | Lumeros(_) => self->dispatch(UnloadTo(facility))
                    | _ => () // Nothing else can casa receive
                    }
                | None => () // Nothing to unload
                }
            | EvedamiaField(_) => () // EvedamiaField cannot unload resources
            }
        | None => () // Do nothing
        }
    }
}

let getCell = (route: array<Messages.cell>, cellId) => route->Array.find((Cell(id, _)) => id == cellId)

let isNeibour = (cellId1: Messages.cellId, cellId2: Messages.cellId) => {
    let dx = abs(cellId1.x - cellId2.x)
    let dy = abs(cellId1.y - cellId2.y)
    dx + dy <= 1
}

let getNextCell = (route: array<Messages.cell>, currentCellId) => {
    let curStep = route->Array.findIndexOpt((Cell(cellId, _)) => currentCellId == cellId)
    
    switch curStep {
    | Some(step) => {
        let nextStep = step === Array.length(route) - 1 ? 0 : step + 1
        route[nextStep]
    }
    | None => None
  }
}

let getCellId = (cell: Messages.cell) => {
  let Messages.Cell(id, _) = cell
  id
}

let make = (patron, id, currentCell) => spawn(~name=String.make(id), patron, async (state: truck, msg, ctx) =>
    switch msg {
    | Messages.StartRoute(route) => {
        let curStep = Option.getOr(
          route->Array.findIndexOpt((cell) => cell === state.position),
          0,
        )
    
        let Messages.Cell(startId, startActor) = Option.getExn(route[curStep], ~message="Invalid route")

        let Messages.Cell(nextId,_) = Option.getExn(getNextCell(route, startId), ~message="Invalid route")

        startActor->dispatch(VehicleVisit(Reply((_) => {
            ctx.self->dispatch(DriveTo(nextId))
        })))

        {
          ...state,
          position: Messages.Cell(startId, startActor),
          movement: Some(Movement(startId, nextId)),
          route: Some(route),
        }
      }
    | SwitchCellTo(toCellId) => {
      let Messages.Cell(curId,_) = state.position
    
      if(toCellId == curId) {
          if(isNeibour(curId, toCellId)) {
            let route = Option.getExn(state.route, ~message="No route set")
              let cell = getCell(route, toCellId)
              
              switch cell {
                | Some(Cell(cellId, cellActor)) => {
                  cellActor->dispatch(VehicleVisit(Reply(handleVisitReply(ctx.self, state))))
                  Option.map(
                    getNextCell(route, cellId), 
                    (cell) => ctx.self->dispatch(DriveTo(getCellId(cell)))
                  )->ignore // TODO: double check this

                  {
                    ...state,
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
    | DriveTo(toCellId) => {
      let Messages.Cell(fromCellId,_) = state.position
      timeout(()=>ctx.self->dispatch(SwitchCellTo(toCellId)), defaultSpeedCellsPerSec)
      
      {
        ...state,
        movement: Some(Movement(fromCellId, toCellId)),
      }
    }
    | ReceiveLumeros(lumeros) => {
        ...state,
        load: Some(Lumeros(lumeros)),
      }
    | ReceiveEvedamia(evedamia) => {
        ...state,
        load: Some(Evedamia(evedamia)),
      }
    | ReceiveMoxalin(moxalin) => {
        ...state,
        load: Some(Moxalin(moxalin)),
      }
    | UnloadTo(_) => failwith("TODO")
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