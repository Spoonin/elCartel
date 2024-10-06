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

type cellId = { x:int, y:int }

type casaMsg = 
| ...buildMsg
| ReceiveLumeros(lumeros)

type player = Player(playerId, actorRef<playerMsg>)

type rec facility = 
| Casa(actorRef<casaMsg>)
| EvedamiaField(actorRef<evedamiaFieldMsg>)

and reason = 
| UnloadResources(facility)
| LoadResources(facility)


and cellMsg = 
| BuildCasa(playerId)
| BuildEvedamiaField(playerId)
| VehicleVisit(reply<option<reason>>)

and cell = Cell(cellId, actorRef<cellMsg>)

and truckMsg = 
| ...receiveResourcesMsg
| UnloadTo(facility)
| SwitchCellTo(cellId)
| StartRoute(array<cell>) // TODO: add cells with explicit stops
| DriveTo(cellId) // TODO: add variable speed

and evedamiaFieldMsg =
| ...buildMsg
| TruckCanLoad(int, actorRef<truckMsg>)
| HarvestEvedamia
