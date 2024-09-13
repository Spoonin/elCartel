open Nact

let system = start()

type rec msgType = Msg(actorRef<msgType>, string)

let wait = (timeout) => Js.Promise.make((~resolve, ~reject as _) => {
  Js.Global.setTimeout(resolve, timeout)->ignore;
})

let ping: actorRef<msgType> = spawnStateless(~name="ping", system, async (Msg(sender, msg), ctx) => {
  Js.log(msg)
  await wait(100)
  sender->dispatch(Msg(ctx.self, ctx.name))
})

let pong: actorRef<msgType> = spawnStateless(~name="pong", system, async (Msg(sender, msg), ctx) => {
  Js.log(msg)
  await wait(100)
  sender->dispatch(Msg(ctx.self, ctx.name))
})

ping->dispatch(Msg(pong, "hello"))

Js.Global.setTimeout(() => {
  Js.log("STOP")
  stop(system)
}, 1000)->ignore