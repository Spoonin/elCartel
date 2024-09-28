open Nact
open Glob
open Cell

type truckId = TruckId(string)

let lumerosCapacity = Lumeros(1000000)
let evedamiaCapacity = Evedamia(1000)
let moxalinCapacity = Moxalin(1000)

type actionStop = Loading

type routePoint = RoutePoint(cell, option<actionStop>)

type msg = 
| ...receiveResourcesMsg
| UnloadResources
| CirculateRoute(array<routePoint>)
| MoveTo(routePoint)
| UpdateRoute(array<routePoint>)
| StopForLoading(float)

type resource = 
    | Lumeros(lumeros)
    | Evedamia(evedamia)
    | Moxalin(moxalin)

type truck = {
    id: truckId,
    load?: option<resource>,
    position: cell,
    route?: option<array<routePoint>>,
    updatedRoute?: option<array<routePoint>>,
    isCirculating: bool,
    isLoading: bool,
}

let make = (patron, truckInitState: truck) => spawn(~name=String.make(truckInitState.id), patron, async (state: truck, msg, _) =>
    switch msg {
    | CirculateRoute(route) => {
        let curStep = Option.getExn(Array.findIndexOpt(route, (RoutePoint(c, _)) => c === state.position), "Truck is not in the route")
        let beginningCell = Option.getExn(route->Array.get(0), "Route is empty")

        let RoutePoint(nextCell, stopReason) = route->Option.getOr(Array.get(curStep + 1), beginningCell)
        nextCell->dispatch(
            switch stopReason {
            | None => {
                TruckVisitPassBy(Reply(movingTime => movingTime->timeout(() => ctx.self->dispatch(MoveTo(state.nextCell)))))
                {
                    ...state,
                    route: Some(route),
                    isCirculating: true
                }
            }
            | Some(_) => {
                TruckVisitWithStop(Reply(stopTime => stopTime->timeout(() => ctx.self->dispatch(StopForLoading(stopTime)))))
                {
                    ...state,
                    route: Some(route),
                    isCirculating: false
                }
            }
            }
        )
    }
    | UnloadResources => {
        switch state.load {
        | Some(Lumeros(l)) => {
            state.position->dispatch(ReceiveLumeros(l))
            { ...state, load: None }
        }
        | Some(Evedamia(e)) => {
            state.position->dispatch(ReceiveEvedamia(e))
            { ...state, load: None }
        }
        | Some(Moxalin(m)) => {
            state.position->dispatch(ReceiveMoxalin(m))
            { ...state, load: None }
        }
        }
    }
    | ReceiveLumeros(lumeros) => {
        { 
         ...state,
         load: Some(Lumeros(lumeros)) 
        }
    }
    | ReceiveEvedamia(evedamia) => 
        { 
         ...state,
         load: Some(Evedamia(evedamia)) 
        }
    | ReceiveMoxalin(moxalin) => 
        { 
         ...state,
         load: Some(Moxalin(moxalin)) 
        }
    | MoveTo(_) => failwith("TODO")
    | UpdateRoute(route) => {
        { 
         ...state,
         updatedRoute: Some(route) 
        }
    }
    }, 
    _ => truckInitState
)