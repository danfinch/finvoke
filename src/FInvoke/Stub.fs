
module internal FInvoke.Stub

open System
open System.Reflection
open Futility

let private oneFunc<'a, 'r> (f : obj array -> obj) =
  { new FSharpFunc<'a, 'r> () with
      member self.Invoke a =
        f [| a |] :?> 'r
  }

let private twoFunc<'a1, 'a2, 'r> (f : obj array -> obj) =
  { new OptimizedClosures.FSharpFunc<'a1, 'a2, 'r> () with
      member self.Invoke a =
        oneFunc<'a2, 'r>
        <| fun aa -> Array.concat [ [| a :> obj |]; aa ] |> f
        :> obj
        :?> 'a2 -> 'r
      member self.Invoke (a1, a2) =
        f [| a1; a2 |] :?> 'r
  }

let private threeFunc<'a1, 'a2, 'a3, 'r> (f : obj array -> obj) =
  { new OptimizedClosures.FSharpFunc<'a1, 'a2, 'a3, 'r> () with
      member self.Invoke a =
        twoFunc<'a2, 'a3, 'r>
        <| fun aa -> Array.concat [ [| a :> obj |]; aa ] |> f
        :> obj
        :?> 'a2 -> 'a3 -> 'r
      member self.Invoke (a1, a2, a3) =
        f [| a1; a2; a3 |] :?> 'r
  }

let private fourFunc<'a1, 'a2, 'a3, 'a4, 'r> (f : obj array -> obj) =
  { new OptimizedClosures.FSharpFunc<'a1, 'a2, 'a3, 'a4, 'r> () with
      member self.Invoke a =
        threeFunc<'a2, 'a3, 'a4, 'r>
        <| fun aa -> Array.concat [ [| a :> obj |]; aa ] |> f
        :> obj
        :?> 'a2 -> 'a3 -> 'a4 -> 'r
      member self.Invoke (a1, a2, a3, a4) =
        f [| a1; a2; a3; a4 |] :?> 'r
  }

let private isChained (t : Type) =
  if t.IsGenericType && (t.GetGenericArguments ()).Length = 2 then
    let afunc = typedefof<FSharpFunc<_,_>>.MakeGenericType (t.GetGenericArguments ())
    afunc.IsAssignableFrom t
  else
    false

let private fiveFunc<'a1, 'a2, 'a3, 'a4, 'a5, 'r> (f : obj array -> obj) =
  let chained = isChained typeof<'r>
  if chained then failwith "Function stubs exceeding 5 parameters not yet supported."
  { new OptimizedClosures.FSharpFunc<'a1, 'a2, 'a3, 'a4, 'a5, 'r> () with
      member self.Invoke a =
        fourFunc<'a2, 'a3, 'a4, 'a5, 'r>
        <| fun aa -> Array.concat [ [| a :> obj |]; aa ] |> f
        :> obj
        :?> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'r
      member self.Invoke (a1, a2, a3, a4, a5) =
        f [| a1; a2; a3; a4; a5 |] :?> 'r
  }

type private stub = class end
let private this = typeof<stub>.DeclaringType
let private oneMethod = this.GetMethod ("oneFunc", BindingFlags.NonPublic ||| BindingFlags.Static)
let private twoMethod = this.GetMethod ("twoFunc", BindingFlags.NonPublic ||| BindingFlags.Static)
let private threeMethod = this.GetMethod ("threeFunc", BindingFlags.NonPublic ||| BindingFlags.Static)
let private fourMethod = this.GetMethod ("fourFunc", BindingFlags.NonPublic ||| BindingFlags.Static)
let private fiveMethod = this.GetMethod ("fiveFunc", BindingFlags.NonPublic ||| BindingFlags.Static)

let create (t : Type) (f : obj array -> obj) =
  let t =
    if t.BaseType == typeof<obj> then
      t
      else
      t.BaseType
  let args = t.GetGenericArguments ()
  let m =
    match args.Length with
    | 2 -> oneMethod
    | 3 -> twoMethod
    | 4 -> threeMethod
    | 5 -> fourMethod
    | 6 -> fiveMethod
    | _ -> failwith ("Inconceivable: " + (string t.BaseType))
  (m.MakeGenericMethod args).Invoke (null, [| f |])

let isFunc (t : Type) =
  if not t.IsGenericType then
    false
  else
    let isf (t : Type) =
      if t.IsGenericType then
        let gt = t.GetGenericTypeDefinition ()
        gt == typedefof<FSharpFunc<_,_>>
        || gt == typedefof<OptimizedClosures.FSharpFunc<_,_,_>>
        || gt == typedefof<OptimizedClosures.FSharpFunc<_,_,_,_>>
        || gt == typedefof<OptimizedClosures.FSharpFunc<_,_,_,_,_>>
        || gt == typedefof<OptimizedClosures.FSharpFunc<_,_,_,_,_,_>>
      else
        false
    if isNull t.BaseType then isf t
    else isf t || isf t.BaseType

