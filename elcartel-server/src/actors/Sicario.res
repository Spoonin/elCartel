open Nact

let salary = Types.Lumeros(10)
let salaryEvery = 2 * minute
let considerBetrayalEvery = 5 * minute
let getBurriedTime = 5 * second
let betrayThreshold = 0.7

type sicario = {
    name: string,
    salariesCount: int,
    guardingCell: option<actorRef<Types.cellMsg>>,
    hapinness: float,
    dead: bool,
    patron: option<actorRef<Player.msg>>
}

let ariphmeticHapinessDecrement = (curHapiness: float) => 1.0 -. curHapiness > 0.0 ? 
1.0 -. curHapiness > 0.5 ? 0.2 : 1.0 -. curHapiness :
0.1

let recallPayDay = async (state, patron: actorRef<Player.msg>, self) => {
    let result = await Glob.query100(patron, (agent) => { #GiveLumeros(salary, agent) })
    
    switch result {
    | Ok(_) =>  self->dispatch(Types.SalaryGiven) // Buy some tequila!
    | Error(_) => self->dispatch(SalaryMissed)
    }

    {
        ...state,
        salariesCount: state.salariesCount + 1
    }
}

let considerBetrayal = (state, patron) => {
    if Js.Math.random() *. 1.0 -. state.hapinness > betrayThreshold {
        patron->dispatch(#SicarioBetrayed(state.name))
        {
            ...state,
            patron: None,
            guardingCell: None
        }
    } else {
        state
    }
}

let make = (name, game) => {
    let self = spawn(~name=name, game, async (state: sicario, msg: Types.sicarioMsg, ctx) => 
    switch msg {
    | RecallPayDay => {
        switch state.patron {
            | Some(patron) => {
                Glob.runPreparation(Duration(salaryEvery), () => ctx.self->dispatch(RecallPayDay))
                await state->recallPayDay(patron, ctx.self)
            }
            | None => state
        }
    }
    | Die => {
        Glob.runPreparation(Duration(getBurriedTime), () => stop(ctx.self))
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
        switch state.patron {
        | Some(patron) => {
            Glob.runPreparation(Duration(considerBetrayalEvery), () => ctx.self->dispatch(ConsiderBetrayal))
            considerBetrayal(state, patron)
        }
        | None => state
        }
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
        guardingCell: None,
        salariesCount: 0,
        hapinness: 1.0,
        dead: false,
        patron: None
        }
    )

    self->dispatch(RecallPayDay)
    self->dispatch(ConsiderBetrayal)

    self
}