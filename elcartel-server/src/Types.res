open Nact

type playerId = PlayerId(string)
type cellId = { x:int, y:int }

type currentSpeed = Speed(float)
type time = Time(float)

type moveDirection = 
    | Up
    | Down
    | Left
    | Right

type error = 
| NotEnoughResources

type updateBuildProcessMsg = UpdateBuildingProcess(float) 

type evedamia = Evedamia(int)
type moxalin = Moxalin(int)
type lumeros = Lumeros(int)

type duration = Duration(int)

type queryRes<'a> = actorRef<'a>

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
| #GiveLumeros(lumeros, queryRes<result<lumeros, string>>) // TODO: protect with token
| #GiveEvedamia(evedamia, queryRes<result<evedamia, error>>) // TODO: protect with token
| #GiveMoxalin(moxalin, queryRes<result<moxalin, error>>) // TODO: protect with token
]

type playerMsg = [
| resourcesExchangeMsg
| #LumerosNotPaid(lumeros, playerId)
| #LumerosDebtedByDealer(lumeros, string)
| #EvedamiaNotProvided(evedamia, playerId)
| #MoxalinNotProvided(moxalin, playerId)
| #HireSicario
| #SicarioBetrayed(string)
| #SicarioDied(string)
]

type player = Player(playerId, actorRef<playerMsg>)

type sicarioMsg = 
| RecallPayDay
| ConsiderBetrayal
| SalaryGiven
| SalaryMissed
| Die

type sicario = Sicario(string, actorRef<sicarioMsg>)

type casaMsg = 
| ...buildMsg
| ReceiveLumeros(lumeros)

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
| #ExchangeEvedamia(evedamia, queryRes<lumeros>)
| #ExchangeMoxalin(moxalin, queryRes<lumeros>)
]

and reason = 
| UnloadResources(facility)
| LoadResources(facility)
| SellResources(facility)


and cellMsg = 
| InitialCasa(player)
| BuildCasa(playerId)
| BuildEvedamiaField(playerId)
| VehicleVisit(queryRes<option<reason>>)

and cell = Cell(cellId, actorRef<cellMsg>)

and truckMsg = [
| receiveResourcesMsg
| #SwitchCellTo(cellId)
| #StartRoute(array<cell>, bool) // TODO: add cells with explicit stops
| #DriveTo(cellId) // TODO: add variable speed
]

and evedamiaFieldMsg =
| ...buildMsg
| TruckCanLoad(int, actorRef<truckMsg>)
| HarvestEvedamia
