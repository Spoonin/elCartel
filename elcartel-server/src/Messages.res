open Glob
open Nact

type buildMsg =
| Build
| UpdateBuildingProcess(float)

type receiveLumerosMsg = ReceiveLumeros(lumeros)

type receiveResourcesMsg = 
| ...receiveLumerosMsg
| ReceiveEvedamia(evedamia)
| ReceiveMoxalin(moxalin)

type resourcesExchangeMsg =
| ...receiveResourcesMsg
| GiveLumeros(lumeros, reply<result<lumeros, error>>)
| GiveEvedamia(evedamia, reply<result<evedamia, error>>)
| GiveMoxalin(moxalin, reply<result<moxalin, error>>)

type playerMsg =
| ...resourcesExchangeMsg
| LumerosNotPaid(lumeros, playerId)
| EvedamiaNotProvided(evedamia, playerId)
| MoxalinNotProvided(moxalin, playerId)
| SicarioBetrayed(string)



type casaMsg = 
| ...buildMsg
| ReceiveLumeros(lumeros)

type rec facility = 
| Casa(actorRef<casaMsg>)
| EvedamiaField(actorRef<evedamiaFieldMsg>)

and reason = 
| UnloadResources(facility)
| LoadResources(facility)
| NoFacility

and vehicleStopOptions = {
    passThruTime: float,
    stopTime: float,
    reason
}

and cellMsg = 
| BuildCasa
| VehicleVisitPassThru(reply<float>)
| VehicleVisitWithStop(reply<vehicleStopOptions>)
| BuildEvedamiaField

and routePoint = RoutePoint(actorRef<cellMsg>, bool)

and truckMsg = 
| ...receiveResourcesMsg
| UnloadTo(facility)
| StartRoute(array<routePoint>)
| MoveTo(routePoint)
| Drive(moveDirection, currentSpeed)
| Stop

and evedamiaFieldMsg =
| ...buildMsg
| TruckCanLoad(int, actorRef<truckMsg>)
| HarvestEvedamia
