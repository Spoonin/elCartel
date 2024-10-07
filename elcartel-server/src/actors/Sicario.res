open Nact
open Glob
open Cell

let salary = Lumeros(10)
let salaryEvery = 2 * minute
let considerBetrayalEvery = 5 * minute
let getBurriedTime = 5 * second
let betrayThreshold = 0.7

type sicario = {
    name: string,
    salariesCount: int,
    guardingCell: option<cell>,
    hapinness: float,
    dead: bool,
}

type msg =
| RecallPayDay
| ConsiderBetrayal
| SalaryGiven
| SalaryMissed
| Die

let ariphmeticHapinessDecrement = (curHapiness: float) => 1.0 -. curHapiness > 0.0 ? 
1.0 -. curHapiness > 0.5 ? 0.2 : 1.0 -. curHapiness :
0.1

let recallPayDay = (state, patron: actorRef<Player.msg>, self) => {
    patron->dispatch(#GiveLumeros(salary, Reply((result) => switch result {
    | Ok(_) =>  self->dispatch(SalaryGiven) // Buy some tequila!
    | Error(NotEnoughResources) => self->dispatch(SalaryMissed)
    })))

    {
        ...state,
        salariesCount: state.salariesCount + 1
    }
}

let considerBetrayal = (state, patron, self) => {
    if Js.Math.random() *. 1.0 -. state.hapinness > betrayThreshold {
        patron->dispatch(#SicarioBetrayed(state.name))
        stop(self)
    }
    state
}

let make = (patron: actorRef<Messages.playerMsg>, name, originCell) => {
    let self = spawn(~name=name, patron, async (state: sicario, msg, ctx) => 
    switch msg {
    | RecallPayDay => {
        runPreparation(Duration(salaryEvery), () => ctx.self->dispatch(RecallPayDay))
        state->recallPayDay(patron, ctx.self)
    }
    | Die => {
        runPreparation(Duration(getBurriedTime), () => stop(ctx.self))
        {
            ...state,
            dead: true
        }
    }
    | SalaryMissed =>{
        let updHapiness = state.hapinness -. ariphmeticHapinessDecrement(state.hapinness)
        {
            ...state,
            hapinness: updHapiness > 0.0 ? updHapiness : 0.0
        }
    }
    | ConsiderBetrayal => {
        runPreparation(Duration(considerBetrayalEvery), () => ctx.self->dispatch(ConsiderBetrayal))
        considerBetrayal(state, patron, ctx.self)
    }
    | SalaryGiven => {
        let updHapiness = state.hapinness +. 0.5
        {
            ...state,
            hapinness: updHapiness < 1.0 ? updHapiness : 1.0
        }
    }},
     _ => {
        name,
        guardingCell: Some(originCell),
        salariesCount: 0,
        hapinness: 1.0,
        dead: false
        }
    )

    self->dispatch(RecallPayDay)
    self->dispatch(ConsiderBetrayal)

    self
}