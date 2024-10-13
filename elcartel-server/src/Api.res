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


// Helper to convert data into JSON responses
let jsonResponse = (data: Js.Json.t, ~status=200) => {
  let headers = Fetch.Headers.fromArray([("Content-Type", "application/json")])
  let dataString = JSON.stringify(data)
  Js.Promise.resolve(
    Fetch.Response.make(Fetch.Body.string(dataString), {headers, status}),
  )
}

let err404 = jsonResponse(Js.Json.object_(Dict.fromArray([("error", Js.Json.string("Not Found"))])), ~status=404)

// Handler for different routes
let handleRequest = (req: Fetch.Request.t) => {
  let url = URL.make(Fetch.Request.url(req))
  switch url["pathname"] {
  | "api/v1/players" => {
    switch Fetch.Request.method(req){
    | #GET => {
      let data = Js.Json.object_(Dict.fromArray([("message", Js.Json.string("Hello, world!"))]))
      jsonResponse(data)
    }
    | _ => err404
    }
  }
  | _ => err404
  }
}

let start = () => {
  // Start the server on port 3000
  serve({
    port: 3000,
    fetch: handleRequest,
    error: (. error) => {
      Js.log2("Server error: ", error)
    },
  })
}

Js.log("Bun server is running on http://localhost:3000")