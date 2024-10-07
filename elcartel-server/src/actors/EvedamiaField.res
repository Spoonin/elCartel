open Nact
open Glob

let installationPrice = Lumeros(2000)

let storageCapacity = 4000
let defaultWeatherFactor = 1.0
let buildTime = 20 * second

let harvestPeriod = 2 * minute

type msg = Messages.evedamiaFieldMsg

type state = {
    buildProcess: float,
    storedEvedamia: int,
    weatherFactor: float,
    currentProductivity: float,
}

let stopTime = 20.0 *. Float.fromInt(second)

let make = (cell, ownPlayer: actorRef<Messages.playerMsg>, evedamiaProductivity) => spawn(~name=`evedamiaField@${String.make(cell)}`, cell, async (state, msg:msg, ctx) =>
    switch msg {
    | Build => {
        ownPlayer->dispatch(
        #GiveLumeros(
          installationPrice,
          Reply(
            result =>
              switch result {
              | Ok(_) => startBuildProcess( // Burn these Lumeros
                  (updBuild: float) => ctx.self->dispatch(UpdateBuildingProcess(updBuild)),
                  buildTime,
                )
              | Error(NotEnoughResources) => Js.log("Not enough resources")
              },
          ),
        ),
        )
        state
    }
    | HarvestEvedamia => {
        if(state.buildProcess >= 1.0) {
            let harvested = Int.fromFloat(state.weatherFactor *. evedamiaProductivity)
            let sumAmount = state.storedEvedamia + harvested

            {
                ...state,
                storedEvedamia: sumAmount > storageCapacity ? storageCapacity : sumAmount,
            }
        } else {
            state
        }
        
    }
    | TruckCanLoad(howMuch, truck) => {
        let loadAmount = state.storedEvedamia > howMuch ? howMuch : state.storedEvedamia
        truck->dispatch(#ReceiveEvedamia(Evedamia(loadAmount)))
        {
            ...state,
            storedEvedamia: state.storedEvedamia - loadAmount,
        }
    }
    | UpdateBuildingProcess(buildProcess) => { ...state, buildProcess }
    }, 
    _ => {
        buildProcess: 0.0,
        storedEvedamia: 0,
        weatherFactor: defaultWeatherFactor,
        currentProductivity: evedamiaProductivity,
    },
)