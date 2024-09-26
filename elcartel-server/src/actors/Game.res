open Nact
open Glob

type msg = 
| Start({users: array<user>})
| End

type rec msgType = Msg(actorRef<msgType>, msg)

type game = {
    cells: array<actorRef<Cell.msg>>
}