open Nact
open Glob
open Cell

type truckId = TruckId(string)

let lumerosCapacity = Lumeros(1000000)
let evedamiaCapacity = Evedamia(1000)
let moxalinCapacity = Moxalin(1000)

type actionStop = Loading

type routePoint = RoutePoint(cellId, option<actionStop>)

type msg = 
| ...receiveResourcesMsg
| UnloadResources
| CirculateRoute(array<routePoint>)
| MoveTo(routePoint)
| BeingAttacked

type resource = 
    | Lumeros(lumeros)
    | Evedamia(evedamia)
    | Moxalin(moxalin)

type truck = {
    id: truckId,
    load?: option<resource>,
    position: cellId,
    nextCell?: option<cellId>,
    route?: option<array<routePoint>>,
    updatedRoute?: option<array<routePoint>>,
    isCirculating: bool,
    isUnderAttack: bool,
}

let startCirculateRouteProcess = (game, , buildTime, ~stagesCount=10) => {
    let rec loop = (prevProcess: float) => {
        if prevProcess < 1.0 {
        Js.Global.setTimeout(() => loop(prevProcess +. Belt.Int.toFloat(1/stagesCount)), buildTime / stagesCount)->ignore
        updateBuildProcess(prevProcess)
        } else {
            updateBuildProcess(1.0)
        }
    }
    loop(0.0)
}

let make = (game, patron, truckInitState: truck) => spawn(~name=String.make(truckInitState.id), patron, async (state: truck, msg, _) =>
    switch msg {
    | CirculateRoute(route) => {
        { 
         ...state,
         route: Some(route), 
         isCirculating: true 
        }
    }
    | UnloadResources => failwith("TODO")
    | ReceiveLumeros(_) => failwith("TODO")
    | ReceiveEvedamia(_) => failwith("TODO")
    | ReceiveMoxalin(_) => failwith("TODO")
    | BeingAttacked => failwith("TODO")
    | MoveTo(_) => failwith("TODO")
    }, 
    _ => truckInitState
)