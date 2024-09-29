open Nact
open Glob


type insufficientError = InsufficientLumerosError | InsufficientEvedamiaError | InsufficientMoxalinError
type didntPay = LumerosNotPaidError | EvedamiaNotProvidedError | MoxalinNotProvidedError

type resources = { 
    evedamia: evedamia, 
    moxalin: moxalin, 
    lumeros: lumeros,
}

type msg = Messages.playerMsg

type player = { playerId, resources, debts: Map.t<playerId, resources>}

let validateLumerosUpd = ({resources}, Lumeros(x)) => {
    let Lumeros(cur) = resources.lumeros
    cur + x >= 0
}

let validateMoxalinUpd = ({resources}, Moxalin(x)) => {
    let Moxalin(cur) = resources.moxalin
    cur + x >= 0
}

let validateEvedamiaUpd = ({resources}, Evedamia(x)) => {
    let Evedamia(cur) = resources.evedamia
    cur + x >= 0
}

let addLumeros = (state, Lumeros(x)) => {
    let Lumeros(curCount) = state.resources.lumeros
    
    {
        ...state,
        resources: {
            ...state.resources,
            lumeros: Lumeros(curCount + x)
        }
    }
}

let addEvedamia = (state, Evedamia(x)) => {
    let Evedamia(curCount) = state.resources.evedamia

    {
        ...state,
        resources: {
            ...state.resources,
            evedamia: Evedamia(curCount + x)
        }
    }
}

let addMoxalin = (state, Moxalin(x)) => {
    let Moxalin(curCount) = state.resources.moxalin

    {
        ...state,
        resources: {
            ...state.resources,
            moxalin: Moxalin(curCount + x)
        }
    }
}

let showErrorToClientSE = (error) => {
    Js.log(error) // TODO: send error to client
}

let make = (game, PlayerId(pId)) => spawn(~name=pId, game, async (state, msg:msg, _) =>
    switch msg {
        | ReceiveLumeros(l) => state->addLumeros(l)
        | ReceiveEvedamia(e) => state->addEvedamia(e)
        | ReceiveMoxalin(m) => state->addMoxalin(m)

        | GiveLumeros(Lumeros(l), Reply(replyTo)) => {
            let realL = Lumeros(l * -1)
            if state->validateLumerosUpd(realL) {
                replyTo(Ok(Lumeros(l)))
                state->addLumeros(realL)
            } else {
                showErrorToClientSE(InsufficientLumerosError)
                state
            }
        }
        | GiveEvedamia(Evedamia(e), Reply(replyTo)) => {
            let realE = Evedamia(e * -1)
            if state->validateEvedamiaUpd(realE) {
                replyTo(Ok(Evedamia(e)))
                state->addEvedamia(realE)
            } else {
                replyTo(Error(NotEnoughResources))
                showErrorToClientSE(InsufficientEvedamiaError)
                state
            }
        }
        | GiveMoxalin(Moxalin(m), Reply(replyTo)) => {
            let realM = Moxalin(m * -1)
            if state->validateMoxalinUpd(realM) {
                replyTo(Ok(Moxalin(m)))
                state->addMoxalin(realM)
            } else {
                replyTo(Error(NotEnoughResources))
                showErrorToClientSE(InsufficientMoxalinError)
                state
            }
        }
        | LumerosNotPaid(Lumeros(l), playerId) => {
            showErrorToClientSE(LumerosNotPaidError)
            switch state.debts->Map.get(playerId) {
                | None => {
                    let debt = {evedamia: Evedamia(0), moxalin: Moxalin(0), lumeros: Lumeros(l)}
                    state.debts->Map.set(playerId, debt)
                    state
                }
                | Some(debt) => {
                    let Lumeros(debtL) = debt.lumeros
                
                    let newDebt = {...debt, lumeros: Lumeros(debtL + l)}
                    state.debts->Map.set(playerId, newDebt)
                    state
                }
            }
        }
        | EvedamiaNotProvided(Evedamia(e), playerId) => {
            showErrorToClientSE(EvedamiaNotProvidedError)
            switch state.debts->Map.get(playerId) {
                | None => {
                    let debt = {evedamia: Evedamia(e), moxalin: Moxalin(0), lumeros: Lumeros(0)}
                    state.debts->Map.set(playerId, debt)
                    state
                }
                | Some(debt) => {
                    let Evedamia(debtE) = debt.evedamia
                
                    let newDebt = {...debt, evedamia: Evedamia(debtE + e)}
                    state.debts->Map.set(playerId, newDebt)
                    state
                }
            }
        }
        | MoxalinNotProvided(Moxalin(m), playerId) => {
            showErrorToClientSE(MoxalinNotProvidedError)
            switch state.debts->Map.get(playerId) {
                | None => {
                    let debt = {evedamia: Evedamia(0), moxalin: Moxalin(m), lumeros: Lumeros(0)}
                    state.debts->Map.set(playerId, debt)
                    state
                }
                | Some(debt) => {
                    let Moxalin(debtM) = debt.moxalin
                
                    let newDebt = {...debt, moxalin: Moxalin(debtM + m)}
                    state.debts->Map.set(playerId, newDebt)
                    state
                }
            }
        }
        | SicarioBetrayed(sicarioId) => {
            Js.log(`Sicario ${sicarioId} betrayed`)
            state
        }
    },
    _ => {
        playerId: PlayerId(pId),
        resources: {
            lumeros: Lumeros(10000),
            evedamia: Evedamia(0),
            moxalin: Moxalin(0)
        },
        debts: Map.make(),
    }
)
