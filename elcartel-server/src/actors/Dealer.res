open Nact

type dealerState = { // TODO: for POC purposes, we imagine that dealer has infinite capacity and amount of lumeros
    name: string,
    rates: Types.marketRates,
    interest: float // TODO: for start just set it to 1.1
}

let make = (name, initialRates, Types.Cell(id, originCell)) => 
  spawn(~name=`${name}@${Cell.idToString(id)}`, originCell, async (state, msg, ctx) => 
    switch msg {
          | #UpdateMarketRates(rates) => {
              ...state,
              rates,
            }
          | #ReceiveLumeros(Types.Lumeros(x)) => {
            Js.log(`${ctx.name} Received ${Int.toString(x)} lumeros`)
            state // TODO: for start just burn this lumeros
          }
          | #ReceiveEvedamia(Types.Evedamia(x)) => {
            Js.log(`${ctx.name} Received ${Int.toString(x)} evedamia`)
            state // TODO: for start just burn this evedamia
          }
          | #ReceiveMoxalin(Types.Moxalin(x)) => {
            Js.log(`${ctx.name} Received ${Int.toString(x)} moxalin`)
            state // TODO: for start just burn this moxalin
          }
          | #GiveLumeros(lumeros, agent) => {
              agent->dispatch(Ok(lumeros)) // infinite amount we can give
              state
            }
          | #GiveEvedamia(_, _) => failwith("TODO")
          | #GiveMoxalin(_, _) => failwith("TODO")
          | #ExchangeEvedamia(Types.Evedamia(amt), agent) => {
            let marketRate = state.rates.evedamiaToLumeros

            let lAmt = Float.fromInt(amt) *. marketRate *. (1.0 -. state.interest)
            let lumeros = Types.Lumeros(Int.fromFloat(lAmt))

            agent->dispatch(lumeros)

            state
          }
          | #ExchangeMoxalin(Types.Moxalin(amt), agent) => {
            let marketRate = state.rates.moxalinToLumeros

            let lAmt = Float.fromInt(amt) *. marketRate *. state.interest
            let lumeros = Types.Lumeros(Int.fromFloat(lAmt))

            agent->dispatch(lumeros)

            state
          }
    },
    _ => {
        name,
        rates: initialRates,
        interest: 1.1
    }
  )
