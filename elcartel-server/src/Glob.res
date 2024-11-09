open Nact

let query100 = (receiver, agent) => query(~timeout=100, receiver, agent)

let runPreparation = (duration, cb: () => unit) => {
    let Types.Duration(x) = duration

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