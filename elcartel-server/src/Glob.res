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

type evedamia = Evedamia(int)
type moxalin = Moxalin(int)
type lumeros = Lumeros(int)

type duration = Duration(int)

type reply<'a> = Reply('a => unit)

let runPreparation = (duration, cb: () => unit) => {
    let Duration(x) = duration

    Js.Global.setTimeout(cb, x)->ignore
}

type updateBuildProcessMsg = UpdateBuildingProcess(float) 

let startBuildProcess = (updateBuildProcess: (float) => unit, buildTime, ~stagesCount=10) => {
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

let timeout = (cb, time) => Js.Global.setTimeoutFloat(cb, time)->ignore