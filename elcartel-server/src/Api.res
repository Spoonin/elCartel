open Glob
open Game

type serveOptions = {
  port: int,
  fetch: (Fetch.Request.t) => promise<Fetch.Response.t>,
  error: (Js.Json.t) => unit,
}

module URL = {
  @new external make: string => {"pathname": string} = "URL"
}

@module("bun")
external serve: (serveOptions) => unit = "serve"

module JsSet = {
  @send external add: (Js.Set.t<'value>, 'value) => Js.Set.t<'value> = "add"
}

exception WrongDataTypeError(string)
// Helper to convert data into JSON responses
let jsonResponse = (data: 'a, ~status=200) => {
  let headers = Fetch.Headers.fromArray([("Content-Type", "application/json")])
  let dataString = JSON.stringifyAny(data)

  switch(dataString) {
    | Some(data) => Fetch.Response.make(Fetch.Body.string(data), {headers, status})
    | None => Fetch.Response.make(Fetch.Body.none, {status: 500, headers})
  }
}

let ok200 = jsonResponse(Dict.fromArray([("ok", true)]))
let ok201 = (id: string) => jsonResponse(Dict.fromArray([("id", id)]), ~status=201)

let err400 = jsonResponse(Dict.fromArray([("error", "Bad Request")]), ~status=400)
let err404 = jsonResponse(Dict.fromArray([("error", "Not Found")]), ~status=404)
let err409 = (~message="Conflict state") => jsonResponse(Dict.fromArray([("error", message)]), ~status=409)

type playerData = {
  id?: string,
}

@scope("JSON") @val
external  parsePlayerData: string => playerData = "parse"

let decodePlayer = json =>
  switch json {
  | Js.Json.Object(userDict) =>
    switch (userDict->Dict.get("id")) {
    | (Some(String(id))) =>
      Some({
        PlayerId(id)
      })
    | _ => None
    }
  | _ => None
  }

// Handler for different routes
let handleRequest = async (req: Fetch.Request.t) => {
  let url = URL.make(Fetch.Request.url(req))

  switch url["pathname"] {
  | "/api/v1/players" => {
    switch Fetch.Request.method(req){
    | #GET => {
      let data = Js.Dict.keys(gameState.players)

      jsonResponse(data)
    }
    | #POST => {
      if Game.hasStarted(gameState) {
        err409(~message="Game has already started")
      } else {
        let bodyData = await Fetch.Request.json(req)
        let playerId = decodePlayer(bodyData)
        switch playerId {
          | Some(id) => {
            JsSet.add(gameState.playersCandidates, id)->ignore
            let PlayerId(pid) = id
            ok201(pid)
          }
          | None => err400
        }
      }
    }
    | _ => err404
    }
  }
  | "/api/v1/start" => {
    switch Fetch.Request.method(req){
      | #POST => {
        if Game.hasStarted(gameState) {
            ok200
        } else if hasCandidates(gameState) {
          Game.startGame(gameState)
          ok200
        } else {
          err409(~message="No candidates to start the game")
        }
      }
      |_ => err404
    }
  }
  | "/api/v1/end" => {
    Game.endGame(gameState)
    ok200
  }
  | _ => err404
  }
}

let port = 3000

let start = () => {
  // Start the server on port 3000
  serve({
    port,
    fetch: handleRequest,
    error: (. error) => {
      Js.log2("Server error: ", error)
    },
  })
}

Js.log(`Server is running on ${Int.toString(port)}`)

start()