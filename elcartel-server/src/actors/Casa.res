open Nact

let installationPrice = Types.Lumeros(10000)
let buildTime = 60 * second

type state = {
    buildProcess: float,
}

type msg = Types.casaMsg

let firstNames = ["Alejandro", "Andrés", "Carlos", "Diego", "Eduardo", "Fernando", "Francisco", "Gabriel", "Gustavo", "Javier", "José", "Juan", "Luis", "Manuel", "Miguel", "Pablo", "Rafael", "Ricardo", "Santiago", "Sebastián"]
let lastNames = ["García", "Martínez", "González", "Pérez", "Rodríguez", "Sánchez", "Ramírez", "Torres", "Flores", "Castillo", "Vázquez", "Morales", "Ríos", "Jiménez", "Díaz", "Reyes", "Ortiz", "Mendoza", "Cruz", "Castro", "Ruiz", "Vega", "Gutiérrez", "Chávez", "Ramos", "Álvarez", "Aguilar", "Domínguez"]

let getRandomName = (names) => Option.getOr(names[Js.Math.random_int(0, names->Array.length - 1)], "")

let randomName = () => `${getRandomName(firstNames)} ${getRandomName(lastNames)} ${Int.toString(Js.Math.random_int(1, 99999999))}`

let getOrCreateSicario = (mbSicario: option<Nact.actorRef<Types.sicarioMsg>>, gameFlow) => 
  switch mbSicario {
    | Some(sicario) => sicario
    | None => {
      let name = randomName()
      let sicario = Sicario.make(name, gameFlow)
      sicario
    }
  }

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
  _ => { buildProcess: 0.0 },
)