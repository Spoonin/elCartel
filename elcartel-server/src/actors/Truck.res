open Nact
open Glob
open Messages
open Cell

type truckId = TruckId(string)

let lumerosCapacity = 1000000
let evedamiaCapacity = 1000
let moxalinCapacity = 1000

type resource = 
    | Lumeros(lumeros)
    | Evedamia(evedamia)
    | Moxalin(moxalin)

type movement = 
    | Movement(moveDirection, currentSpeed)

type truck = {
    id: truckId,
    load: option<resource>,
    position: cell,
    route: option<array<routePoint>>,
    movement,
    isLoading: bool,
}

let handleVehicleStop = (self, state) => {
    (options: vehicleStopOptions,) => {
        let { passThruTime, reason, stopTime } = options

        switch reason {
        | LoadResources(facility) =>
            switch facility {                
            | EvedamiaField(evedamiaField) =>{
                evedamiaField->dispatch(Messages.TruckCanLoad(evedamiaCapacity, self))
                self->dispatch(Drive(Up, Speed(1.0 /. passThruTime)))
            }
            | Casa(casa) => failwith("TODO")
            }
        | UnloadResources(facility) => 
            switch facility {
            | Casa(casa) =>
                switch state.load {
                | Some(resource) => 
                    switch resource {
                    | Lumeros(lumeros) => self->dispatch(UnloadTo(facility))
                    | Evedamia(evedamia) => failwith("Invalid load type: Evedamia")
                    | Moxalin(moxalin) => failwith("Invalid load type: Evedamia")
                    }
                | None => () // Nothing to unload
                | _ => failwith("TODO")
                }
            | EvedamiaField(_) => failwith("TODO")
            }
        | NoFacility => () // Do nothing
        }
    }
}


let make = (patron, id, currentCell) => spawn(~name=String.make(id), patron, async (state: truck, msg, ctx) =>
    switch msg {
    | StartRoute(route) => {
        let curStep = Option.getOr(
          route->Array.findIndexOpt((RoutePoint(cell, _)) => cell === state.position),
          0,
        )
    
        let newCycle = curStep === Array.length(route) - 1
    
        let curPoint = route[curStep]
    
        let RoutePoint(curPosition, _) = Option.getExn(curPoint, ~message="Route is empty")
    
        let nextPoint = Option.getExn(
          newCycle ? route[0] : route[curStep + 1],
          ~message="Route is empty",
        )
    
        ctx.self->dispatch(MoveTo(nextPoint))
    
        {
          ...state,
          position: curPosition,
          route: Some(route),
        }
      }
    | MoveTo(RoutePoint(cell, isStop)) => {
        switch isStop {
        | true => cell->dispatch(VehicleVisitWithStop(Reply(handleVehicleStop(ctx.self, state))))
        | false => cell->dispatch(
            VehicleVisitPassThru(
              Reply(
                passThruTime => {
                  ctx.self->dispatch(Drive(Up, Speed(1.0 /. passThruTime)))
                },
              ),
            ),
          )
        }
        state
      }
    | Drive(direction, speed) => {
        ...state,
        movement: Movement(direction, speed),
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
    | Stop => {
        let Movement(currentDirection,_) = state.movement
        
        { ...state,
            movement: Movement(currentDirection, Speed(0.0)),
        }
    }
    | UnloadTo(_) => failwith("TODO")
    }, 
    _ => {
        id,
        load: None,
        position: currentCell,
        route: None,
        movement: Movement(Up, Speed(0.0)),
        isLoading: false,
    }
)