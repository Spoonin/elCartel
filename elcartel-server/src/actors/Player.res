open Nact

type insufficient = InsufficientError

type evedamia = Evedamia(int)
type moxalin = Moxalin(int)
type lumeros = Lumeros(int)


type resources = { 
    evedamia: evedamia,
    moxalin: moxalin,
    lumeros: lumeros,
}

type player = { resources: resources }

type updatePlayerMsg =
  | AddLumeros(lumeros)
  | AddEvedamia(evedamia)
  | AddMoxalin(moxalin)


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

let addLumeros = ({resources}, Lumeros(x)) => {
    let Lumeros(curCount) = resources.lumeros
    
    {
        resources: {
            ...resources,
            lumeros: Lumeros(curCount + x)
        }
    }
}

let addEvedamia = ({resources}, Evedamia(x)) => {
    let Evedamia(curCount) = resources.evedamia

    {
        resources: {
            ...resources,
            evedamia: Evedamia(curCount + x)
        }
    }
}

let addMoxalin = ({resources}, Moxalin(x)) => {
    let Moxalin(curCount) = resources.moxalin

    {
        resources: {
            ...resources,
            moxalin: Moxalin(curCount + x)
        }
    }
}

let make = (game, playerId) => spawn(~name=playerId, game, async (state: player, (sender, msg), _) =>
    switch msg {
        | AddLumeros(l) => if state->validateLumerosUpd(l) {
                sender->dispatch(Ok())
                state->addLumeros(l)
            } else {
                sender->dispatch(Error(InsufficientError))
                state
            }
        | AddEvedamia(e) => if state->validateEvedamiaUpd(e) {
                sender->dispatch(Ok())
                state->addEvedamia(e)
            } else {
                sender->dispatch(Error(InsufficientError))
                state
            }
        | AddMoxalin(m) => if state->validateMoxalinUpd(m) {
                sender->dispatch(Ok())
                state->addMoxalin(m)
            } else {
                sender->dispatch(Error(InsufficientError))
                state
            }
    },
    _ => {resources: {
        lumeros: Lumeros(1000),
        evedamia: Evedamia(0),
        moxalin: Moxalin(0)
    }}
)