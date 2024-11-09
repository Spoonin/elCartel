open Nact

let installationPrice = Types.Lumeros(10000)
let buildTime = 60 * second

type state = {
    buildProcess: float,
}

type msg = Types.casaMsg


let make = (ownPlayer: actorRef<Types.playerMsg>, cellName: string) => spawn(~name=`casa${cellName}`, ownPlayer, async (state, msg:msg, ctx) =>
  switch msg {
  | Build => {
      let result = await Glob.query100(
        ownPlayer,
        (agent) => { #GiveLumeros(installationPrice, agent) }
      )

      switch result {
      | Ok(_) => Glob.startBuildProcess(// Burn these Lumeros
        (updBuild: float) => ctx.self->dispatch(UpdateBuildingProcess(updBuild)), buildTime)
      | Error(err) => Js.log(err)
      }
      state
    }
  | ReceiveLumeros(lumeros) => {
      ownPlayer->dispatch(#ReceiveLumeros(lumeros))
      state
    }
  | UpdateBuildingProcess(process) => {buildProcess: process}
  },
  _ => { 
    buildProcess: 0.0
  },
)