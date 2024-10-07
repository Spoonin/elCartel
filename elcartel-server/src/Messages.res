open Glob
open Nact

type buildMsg =
| Build
| UpdateBuildingProcess(float)

type receiveLumerosMsg = [#ReceiveLumeros(lumeros)]

type receiveResourcesMsg = [
| receiveLumerosMsg
| #ReceiveEvedamia(evedamia)
| #ReceiveMoxalin(moxalin)
]

type resourcesExchangeMsg = [
| receiveResourcesMsg
| #GiveLumeros(lumeros, reply<result<lumeros, error>>) // Must be called only from Reply TODO: protect with token
| #GiveEvedamia(evedamia, reply<result<evedamia, error>>) // Must be called only from Reply TODO: protect with token
| #GiveMoxalin(moxalin, reply<result<moxalin, error>>) // Must be called only from Reply TODO: protect with token
]

type playerMsg = [
| resourcesExchangeMsg
| #LumerosNotPaid(lumeros, playerId)
| #LumerosDebtedByDealer(lumeros, string)
| #EvedamiaNotProvided(evedamia, playerId)
| #MoxalinNotProvided(moxalin, playerId)
| #SicarioBetrayed(string)
]

type cellId = { x:int, y:int }

type casaMsg = 
| ...buildMsg
| ReceiveLumeros(lumeros)

type player = Player(playerId, actorRef<playerMsg>)

type marketRates = {
    evedamiaToLumeros: float,
    moxalinToLumeros: float
}


type rec facility = 
| Casa(actorRef<casaMsg>)
| EvedamiaField(actorRef<evedamiaFieldMsg>)
| Dealer(actorRef<dealerMsg>)

and dealerMsg = [
| resourcesExchangeMsg
| #UpdateMarketRates(marketRates)
| #ExchangeEvedamia(evedamia, reply<lumeros>)
| #ExchangeMoxalin(moxalin, reply<lumeros>)
]

and reason = 
| UnloadResources(facility)
| LoadResources(facility)
| SellResources(facility)


and cellMsg = 
| BuildCasa(playerId)
| BuildEvedamiaField(playerId)
| VehicleVisit(reply<option<reason>>)

and cell = Cell(cellId, actorRef<cellMsg>)

and truckMsg = [
| receiveResourcesMsg
| #UnloadTo(facility)
| #SwitchCellTo(cellId)
| #StartRoute(array<cell>) // TODO: add cells with explicit stops
| #DriveTo(cellId) // TODO: add variable speed
]

and evedamiaFieldMsg =
| ...buildMsg
| TruckCanLoad(int, actorRef<truckMsg>)
| HarvestEvedamia
