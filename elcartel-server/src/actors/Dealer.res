open Nact
open Glob

type dealerState = { // TODO: for POC purposes, we imagine that dealer has infinite capacity and amount of lumeros
    name: string,
    rates: Messages.marketRates,
    interest: float // TODO: for start just set it to 1.1
}

let make = (name, initialRates, Messages.Cell(id, originCell)) => spawn(~name=`${name}@${Cell.idToString(id)}`, originCell, async (state, msg, ctx) => {
        {
          {
            {
              switch msg {
              | #UpdateMarketRates(rates) => {
                  ...state,
                  rates,
                }
              | #ReceiveLumeros(Lumeros(x)) => {
                Js.log(`${ctx.name} Received ${Int.toString(x)} lumeros`)
                state // TODO: for start just burn this lumeros
              }
              | #ReceiveEvedamia(Evedamia(x)) => {
                Js.log(`${ctx.name} Received ${Int.toString(x)} evedamia`)
                state // TODO: for start just burn this evedamia
              }
              | #ReceiveMoxalin(Moxalin(x)) => {
                Js.log(`${ctx.name} Received ${Int.toString(x)} moxalin`)
                state // TODO: for start just burn this moxalin
              }
              | #GiveLumeros(lumeros, Reply(replyTo)) => {
                  replyTo(Ok(lumeros)) // infinite amount we can give
                  state
                }
              | #GiveEvedamia(_, _) => failwith("TODO")
              | #GiveMoxalin(_, _) => failwith("TODO")
              | #ExchangeEvedamia(Evedamia(amt), Reply(replyTo)) => {
                let marketRate = state.rates.evedamiaToLumeros

                let lAmt = Float.fromInt(amt) *. marketRate *. (1.0 -. state.interest)
                let lumeros = Lumeros(Int.fromFloat(lAmt))

                replyTo(lumeros)

                state
              }
              | #ExchangeMoxalin(Moxalin(amt), Reply(replyTo)) => {
                let marketRate = state.rates.moxalinToLumeros

                let lAmt = Float.fromInt(amt) *. marketRate *. state.interest
                let lumeros = Lumeros(Int.fromFloat(lAmt))

                replyTo(lumeros)

                state
              }
              }
            }
          }
        }
    },
    _ => {
        name,
        rates: initialRates,
        interest: 1.1
    }
    )
