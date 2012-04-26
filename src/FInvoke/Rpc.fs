
namespace FInvoke

// todo: support optional arguments
// todo: support optional service methods
// todo: support classes with non-default constructors
// todo: support async<returntype>
// todo: support methods with more than 5 parameters in F# proxy
// todo: support more bindings than http
// todo: support metadata services (json-smd?)
// todo: support multiple procedure calls in js proxy
// todo: support streaming IEnumerable/IObservable/etc.
// todo: support services with no namespaces (bug)
// todo: allow suppressing debug messages
// todo: allow for skipping mapping and return a dynamic object
// todo: when extensibility is implemented in FSerial, allow custom serializers

open System
open System.Reflection
open System.Net
open System.Net.Sockets
open System.IO
open System.IO.Pipes
open System.Text
open Microsoft.FSharp.Reflection
open Futility
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Bson
open FGateway
open FSerial
open FInvoke
open FInvoke.JsProxy

type Format = {
  Read            : Stream -> JToken
  Write           : Stream -> JToken -> unit
  ContentType     : string
} with
  static member json =
    { ContentType = "application/json"
      Read = fun stream ->
        let tr = new StreamReader (stream)
        let json = new JsonTextReader (tr)
        JValue.ReadFrom json
      Write = fun stream token ->
        let tw = new StreamWriter (stream)
        let json = new JsonTextWriter (tw)
        token.WriteTo json
        json.Flush ()
    }
  static member bson =
    { ContentType = "application/bson"
      Read = fun stream ->
        let bson = new BsonReader (stream)
        JValue.ReadFrom bson
      Write = fun stream token ->
        let bson = new BsonWriter (stream)
        token.WriteTo bson
        bson.Flush ()
    }

module Host =
  module Errors =
    let parseError = -32700
    let invalidRequest = -32600
    let methodNotfound = -32601
    let invalidParams = -32602
    let unknownError = -32000

  exception InvalidRequest of string
  exception InvalidParams of string
  exception MethodNotFound of string
  exception UnknownError of string

  let private buildError (id : string) (code : int) (message : string) : JToken =
    let response = JObject ()
    let error = JObject ()
    response.["jsonrpc"] <- JValue "2.0"
    response.["error"] <- error
    response.["id"] <- JValue id
    error.["code"] <- JValue code
    error.["message"] <- JValue message
    response :> JToken

  let rec private invoke (i : int) (f : obj) (pars : JArray) =
    let ft = f.GetType().BaseType
    let fas = ft.GetGenericArguments ()
    let it, ot = fas.[0], fas.[1]
    let meth =
      ft
      |> Type.getMethods
      |> List.filter (fun m -> m.Name = "Invoke")
      |> List.minBy (fun m -> m.GetParameters().Length)
    let args =
      if meth.GetParameters().Length = 1 && meth.GetParameters().[0].ParameterType == typeof<Unit> then
        [ () :> obj ]
      else
        [ pars.[i] |> ofNode it ]
    let res =
      meth
      |> Method.invoke f args
    match res with
    | null -> null
    | o when o.GetType() |> Stub.isFunc -> invoke (i + 1) o pars
    | x -> x

  let private tryCall (service : obj) (token : JToken) : JToken =
    let respond (req : JObject) =
      if req.["jsonrpc"] |> string <> "2.0" then
        raise (InvalidRequest "Unsupported protocol version.")
      let methodName = req.["method"] |> string
      let prop = service.GetType () |> Type.getProperty methodName
      let func = FSharpValue.GetRecordField (service, prop)
      let reqID = req.["id"] |> string
      let pars =
        match req.["params"].Type with
        | JTokenType.Array ->
          req.["params"] :?> JArray
        | JTokenType.Object ->
          raise (InvalidParams "Named parameters are not supported for this type of service.")
        | _ -> raise (InvalidParams "Arguments must be in the form of an array.")
      let result =
        let res = invoke 0 func pars
        match res with
        | null -> JValue (null :> obj) :> JToken
        | x -> x |> node (x.GetType())
      let response = JObject ()
      response.["jsonrpc"] <- JValue "2.0"
      response.["result"] <- result
      response.["id"] <- JValue reqID
      response :> JToken
    let tryRespond (req : JToken) =
      let req =
        match req.Type with
        | JTokenType.Object -> req :?> JObject
        | _ -> raise (InvalidRequest "Elements of a batch request must be objects.")
      let id = req.["id"] |> string
      try
        respond req
      with
      | InvalidRequest msg -> buildError id Errors.invalidRequest msg
      | InvalidParams msg -> buildError id Errors.invalidParams msg
      | MethodNotFound msg -> buildError id Errors.methodNotfound msg
    let batchRespond (reqs : JArray) : JToken =
      let resps = JArray ()
      for req in reqs do
        resps.Add (tryRespond req)
      resps :> JToken
    match token.Type with
    | JTokenType.Array -> token :?> JArray |> batchRespond
    | JTokenType.Object -> token |> tryRespond
    | _ -> raise (InvalidRequest "Request must take the form of a JSON object or array.")

  /// Executes a JSON-RPC call on the service instance.
  let execute (instance : obj) (token : JToken) =
    try
      tryCall instance token
    with
      | :? JsonSerializationException as e -> buildError null Errors.parseError e.Message
      | MethodNotFound msg -> buildError null Errors.methodNotfound msg
      | InvalidRequest msg -> buildError null Errors.invalidRequest msg
      | e ->
        let rec inner (e : Exception) =
          if e.InnerException <> null then
            inner e.InnerException
          else e
        buildError null Errors.unknownError (inner e).Message

  /// Returns a function which takes an input and output stream and executes a JSON-RPC call on the service instance.
  let handler (instance : obj) (format : Format) =
    let inner input output =
      format.Read input
      |> execute instance
      |> format.Write output
    inner

  /// Starts a TCP listener which executes calls on the service instance.
  let tcp instance format ipAddress port queue threads =
    let handler s = handler instance format s s
    TcpServer.start ipAddress port queue threads handler

  /// Starts a named pipe listener which executes calls on the service instance.
  let namedPipe instance format pipeName threads =
    let handler s = handler instance format s s
    NamedPipeServer.start pipeName threads handler

  /// Returns a function which executes JSON-RPC calls for HTTP POST requests and produces a JavaScript proxy on GET requests.
  let http instance format =
    let handler r = handler instance format r.InputStream r.OutputStream
    let handleCall request =
      request.SetContentType format.ContentType
      handler request
    let proxy = instance |> JsProxy.create ""
    let handleProxy request =
      try
        request.SetContentType "text/javascript"
        request.OutputStream
        |> Stream.writeText proxy
      with e ->
        request.SetStatusCode 500
        request.SetStatusDescription e.Message
    let handle (request : Request) =
      match request.Method |> String.upper with
      | "POST" -> request |> handleCall
      | "GET" -> request |> handleProxy
      | _ -> 
        request.SetStatusCode 405
        request.SetStatusDescription "Invalid HTTP method."
      request
    handle

module Proxy =

  let create (format : Format) (connect : unit -> Stream Lazy * Stream * (unit -> unit)) : 's =
    let id = ref 0
    let intercept (connect : unit -> Stream Lazy * Stream * (unit -> unit)) (pi : PropertyInfo) (ps : obj array) : obj =
      id := !id + 1
      let req = JObject ()
      req.["jsonrpc"] <- JValue "2.0"
      req.["method"] <- JValue pi.Name
      req.["id"] <- JValue (!id)
      let pars = JArray ()
      let args = pi.PropertyType.GetGenericArguments ()
      for a in 0 .. args.Length - 2 do
        pars.Add (node args.[a] ps.[a])
      req.["params"] <- pars
      let input, output, disconnect = connect ()
      format.Write output (req :> JToken)
      let resp = format.Read !input
      disconnect ()
      if resp.["error"] <> null then
        failwith (resp.["error"].["message"] |> string)
      let rtype = args.[args.Length - 1]
      resp.["result"]
      |> ofNode rtype
    let serviceType = typeof<'s>
    let stub (p : PropertyInfo) =
      Stub.create p.PropertyType (intercept connect p)
    let ps =
      FSharpType.GetRecordFields serviceType
      |> Array.map stub
    FSharpValue.MakeRecord (serviceType, ps)
    :?> 's

  let http format timeoutMS (address : string) =
    let connect () =
      let http = WebRequest.Create address
      http.Method <- "POST"
      http.Timeout <- timeoutMS
      lazy http.GetResponse().GetResponseStream()
      , http.GetRequestStream ()
      , fun _ -> ()
    create format connect

  let tcp format timeoutMS (ipAddress : IPAddress) (port : int) =
    let connect () =
      let tcp = new TcpClient ()
      tcp.SendTimeout <- timeoutMS
      tcp.ReceiveTimeout <- timeoutMS
      tcp.Connect (ipAddress, port)
      let s = tcp.GetStream () :> Stream
      lazy s, s, tcp.Close
    create format connect

  let namedPipe format timeoutMS pipeName =
    let connect () =
      let pipe = new NamedPipeClientStream (pipeName)
      pipe.ReadTimeout <- timeoutMS
      pipe.WriteTimeout <- timeoutMS
      pipe.Connect ()
      let pipe = pipe :> Stream
      lazy pipe, pipe, pipe.Dispose
    create format connect
