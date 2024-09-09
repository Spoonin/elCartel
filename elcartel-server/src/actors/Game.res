open Nact

let system = start()

type user = {
    id: string,
    name: string
}

type msg = 
| Start({users: array<user>})
| End

type rec msgType = Msg(actorRef<msgType>, msg)