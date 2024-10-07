open Glob
open Nact

let installationPrice = Lumeros(10000)
let buildTime = 60 * second

type state = {
    buildProcess: float,
}

type msg = Messages.casaMsg
  
let unloadStopTime = 20.0 *. Float.fromInt(second)

let make = (ownPlayer: actorRef<Messages.playerMsg>, cellName: string) => spawn(~name=`casa@${cellName}`, ownPlayer, async (state, msg:msg, ctx) =>
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
  | ReceiveLumeros(lumeros) => {
      ownPlayer->dispatch(#ReceiveLumeros(lumeros))
      state
    }
  | UpdateBuildingProcess(process) => { buildProcess: process }
  },
  _ => { buildProcess: 0.0 },
)