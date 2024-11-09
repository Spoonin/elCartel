open Nact

type insufficientError = InsufficientLumerosError | InsufficientEvedamiaError | InsufficientMoxalinError
type didntPay = LumerosNotPaidError | EvedamiaNotProvidedError | MoxalinNotProvidedError

type resources = { 
    evedamia: Types.evedamia, 
    moxalin: Types.moxalin, 
    lumeros: Types.lumeros,
}

type msg = Types.playerMsg

type playerState = {
    resources, 
    debts: Map.t<Types.playerId, resources>, 
    dealersDepts: Map.t<string, Types.lumeros>,
}

let validateLumerosUpd = ({resources}, Types.Lumeros(x)) => {
    let Types.Lumeros(cur) = resources.lumeros
    cur + x >= 0
}

let validateMoxalinUpd = ({resources}, Types.Moxalin(x)) => {
    let Types.Moxalin(cur) = resources.moxalin
    cur + x >= 0
}

let validateEvedamiaUpd = ({resources}, Types.Evedamia(x)) => {
    let Types.Evedamia(cur) = resources.evedamia
    cur + x >= 0
}

let addLumeros = (state, Types.Lumeros(x)) => {
    let Types.Lumeros(curCount) = state.resources.lumeros
    
    {
        ...state,
        resources: {
            ...state.resources,
            lumeros: Types.Lumeros(curCount + x)
        }
    }
}

let addEvedamia = (state, Types.Evedamia(x)) => {
    let Types.Evedamia(curCount) = state.resources.evedamia

    {
        ...state,
        resources: {
            ...state.resources,
            evedamia: Types.Evedamia(curCount + x)
        }
    }
}

let addMoxalin = (state, Types.Moxalin(x)) => {
    let Types.Moxalin(curCount) = state.resources.moxalin

    {
        ...state,
        resources: {
            ...state.resources,
            moxalin: Types.Moxalin(curCount + x)
        }
    }
}

let showErrorToClientSE = (error) => {
    Js.log(error) // TODO: send error to client
}

let make = (gameFlow, Types.PlayerId(pId)) => spawn(~name=pId, gameFlow, async (state, msg:msg, _) =>
    switch msg {
        | #ReceiveLumeros(l) => state->addLumeros(l)
        | #ReceiveEvedamia(e) => state->addEvedamia(e)
        | #ReceiveMoxalin(m) => state->addMoxalin(m)

        | #GiveLumeros(Types.Lumeros(l), agent) => {
            let realL = Types.Lumeros(l * -1)
            if state->validateLumerosUpd(realL) {
                agent->dispatch(Ok(Lumeros(l)))
                state->addLumeros(realL)
            } else {
                showErrorToClientSE(InsufficientLumerosError)
                agent->dispatch(Error("Not enough resources"))
                state
            }
        }
        | #GiveEvedamia(Types.Evedamia(e), agent) => {
            let realE = Types.Evedamia(e * -1)
            if state->validateEvedamiaUpd(realE) {
                agent->dispatch(Ok(Evedamia(e)))
                state->addEvedamia(realE)
            } else {
                agent->dispatch(Error(NotEnoughResources))
                showErrorToClientSE(InsufficientEvedamiaError)
                state
            }
        }
        | #GiveMoxalin(Types.Moxalin(m), agent) => {
            let realM = Types.Moxalin(m * -1)
            if state->validateMoxalinUpd(realM) {
                agent->dispatch(Ok(Moxalin(m)))
                state->addMoxalin(realM)
            } else {
                agent->dispatch(Error(NotEnoughResources))
                showErrorToClientSE(InsufficientMoxalinError)
                state
            }
        }
        | #LumerosNotPaid(Lumeros(l), playerId) => {
            showErrorToClientSE(LumerosNotPaidError)
            switch state.debts->Map.get(playerId) {
                | None => {
                    let debt = {evedamia: Evedamia(0), moxalin: Moxalin(0), lumeros: Lumeros(l)}
                    state.debts->Map.set(playerId, debt)
                    state
                }
                | Some(debt) => {
                    let Types.Lumeros(debtL) = debt.lumeros
                
                    let newDebt = {...debt, lumeros: Lumeros(debtL + l)}
                    state.debts->Map.set(playerId, newDebt)
                    state
                }
            }
        }
        | #LumerosDebtedByDealer(lumeros, dealerId) => {
            switch state.dealersDepts->Map.get(dealerId) {
                | None => {
                    state.dealersDepts->Map.set(dealerId, lumeros)
                    state
                }
                | Some(debt) => {
                    let Types.Lumeros(debtL) = debt
                    let Types.Lumeros(newDebtL) = lumeros
                
                    let newDebt = Types.Lumeros(debtL + newDebtL)
                    state.dealersDepts->Map.set(dealerId, newDebt)
                    state
                }
            }
        }
        | #EvedamiaNotProvided(Evedamia(e), playerId) => {
            showErrorToClientSE(EvedamiaNotProvidedError)
            switch state.debts->Map.get(playerId) {
                | None => {
                    let debt = {evedamia: Evedamia(e), moxalin: Moxalin(0), lumeros: Lumeros(0)}
                    state.debts->Map.set(playerId, debt)
                    state
                }
                | Some(debt) => {
                    let Types.Evedamia(debtE) = debt.evedamia
                
                    let newDebt = {...debt, evedamia: Evedamia(debtE + e)}
                    state.debts->Map.set(playerId, newDebt)
                    state
                }
            }
        }
        | #MoxalinNotProvided(Moxalin(m), playerId) => {
            showErrorToClientSE(MoxalinNotProvidedError)
            switch state.debts->Map.get(playerId) {
                | None => {
                    let debt = {evedamia: Evedamia(0), moxalin: Moxalin(m), lumeros: Lumeros(0)}
                    state.debts->Map.set(playerId, debt)
                    state
                }
                | Some(debt) => {
                    let Types.Moxalin(debtM) = debt.moxalin
                
                    let newDebt = {...debt, moxalin: Moxalin(debtM + m)}
                    state.debts->Map.set(playerId, newDebt)
                    state
                }
            }
        }

        | #SicarioBetrayed(sicarioId) => {
            Js.log(`Sicario ${sicarioId} betrayed`)
            state
        }
    },
    _ => {
        resources: {
            lumeros: Lumeros(50000),
            evedamia: Evedamia(0),
            moxalin: Moxalin(0)
        },
        debts: Map.make(),
        dealersDepts: Map.make()
    }
)
