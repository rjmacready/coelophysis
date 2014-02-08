module Coelophysis

open Microsoft.FSharp.Text.Lexing
open System
open System.IO
open System.Reflection
open System.Security.Policy
open System.Text
open System.Threading

open AstObject
open Eval
open HttpServer

let write _ (astream : Value) : Value =
    let v = new Value()
    let estream = astream.GetValue()
    (match estream with
    | Any(obj) -> 
        NativeFunction(fun _ (stuff : Value) ->
            let v2 = new Value()
            let estuff = stuff.GetValue()
            let bytes = (match estuff with
                        | Str(s) -> Encoding.UTF8.GetBytes(s)
                        | _ -> failwith "Cant extract bytes ...")
            (match obj with
            | :? Stream as stream -> 
                stream.Write(bytes, 0, bytes.Length)
                estream
            | _ -> failwith "...") |> v2.SetValue)
    | _ -> failwith "...") |> v.SetValue

let close _ (astream : Value) : Value =
    let v = new Value()
    let estream = astream.GetValue()
    (match estream with
    | Any(obj) -> 
        match obj with
            | :? Stream as stream -> 
                stream.Close()
                estream
            | _ -> failwith "..."
    | _ -> failwith "...") |> v.SetValue

let get_clr_property _ (value : Value) : Value =
    let v = new Value()
    let evalue = value.GetValue()
    (match evalue with
    | Any(obj) -> 
        let t = obj.GetType()
        NativeFunction(fun _ (property : Value) ->
                        let v2 = new Value()
                        let eproperty = property.GetValue()
                        (match eproperty with
                        | Symbol(name) -> 
                            let propertyinfo = t.GetProperty(name)
                            Any(propertyinfo.GetValue(obj, [| |]))
                        | _ -> failwith "Give me a symbol!") |> v2.SetValue)
    | _ -> failwith "I can only get properties from Any(obj)") |> v.SetValue

let set_clr_property _ (value : Value) : Value =
    let v = new Value()
    let evalue = value.GetValue()
    (match evalue with
    | Any(obj) -> 
        let t = obj.GetType()
        NativeFunction(fun _ (property : Value) ->
                        let v2 = new Value()
                        let eproperty = property.GetValue()
                        (match eproperty with
                        | Symbol(name) -> 
                            NativeFunction(fun _ (v : Value) ->
                                let v3 = new Value()
                                let ev = v.GetValue()
                                let propertyinfo = t.GetProperty(name)
                                Any(propertyinfo.SetValue(obj, ev, [| |]))
                                (*match ev with
                                | Any(o) -> Any(propertyinfo.SetValue(obj, o, [| |]))
                                | _ -> failwith "..."*) |> v3.SetValue)
                        | _ -> failwith "Give me a symbol!") |> v2.SetValue)
    | _ -> failwith "I can only get properties from Any(obj)") |> v.SetValue

(*
let add_single_event_listener _ (value : Value) : Value =
    let v = new Value()
    let evalue = value.GetValue()
    (match evalue with
    | Any(obj) ->
        let t = obj.GetType()
        NativeFunction(fun _ (event : Value) -> 
                        let v2 = new Value()
                        let eevent = event.GetValue()
                        (match eevent with
                        | Symbol(name) ->
                            let eventinfo = t.GetEvent(name)                       
                            NativeFunction(fun e (fn : Value) ->                                             
                                            let v3 = new Value()
                                            let efn = fn.GetValue()
                                            eventinfo.AddEventHandler(obj, new OnContextDelegate(fun _ ectx ->
                                                let ctx = ectx.Ctx in                                                
                                                let r = Any(ctx) |> Ev.Call e efn in
                                                r.GetValue() |> ignore))
                                            let efn = fn.GetValue()
                                            Unit |> v3.SetValue)
                        | _ -> failwith "Give me a symbol!") |> v2.SetValue)
    | _ -> failwith "I can only add handlers on Any(obj)") |> v.SetValue
    *)

let exit _ (x : Value) : Value =
    x.GetValue() |> ignore
    Environment.Exit(-1)
    Unit |> Value.CreateAndWrap

let outstring (s : TextWriter) (t : String) =
    s.Write(t)
    s.Flush()

let url _ (x : Value) : Value =
    let r = new Value()
    let x' = x.GetValue()
    (match x' with
    | Str(u) -> Any(new Uri(u))
    | _ -> failwith "Argument of url should be a string") |> r.SetValue

let http_server _ (u : Value) : Value =
    let r = new Value()
    let u' = u.GetValue()
    (match u' with
    | Any(rawu) ->
        match rawu with
        | :? Uri as u ->
            let hs = new HttpServer()
            Any(hs.Init [u.ToString()])
        | _ -> failwith "Argument of http_server is not an url"
    | _ -> failwith "Argument of http_server should be an url") |> r.SetValue

let start _ (s : Value) : Value =
    let r = new Value()
    let s' = s.GetValue()
    (match s' with
    | Any(hs) -> 
        match hs with
        | :? HttpServer as s ->
            Any(s.Start())
        | _ -> failwith "Argument of start is not a startable"
    | _ -> failwith "Argument of start should be a startable") |> r.SetValue

let stop _ (s : Value) : Value =
    let r = new Value()
    let s' = s.GetValue()
    (match s' with
    | Any(hs) -> 
        match hs with
        | :? HttpServer as s ->
            Any(s.Stop())
        | _ -> failwith "Argument of stop is not a stopable"
    | _ -> failwith "Argument of stop should be a stopable") |> r.SetValue

let equals _ (x : Value) : Value =
    let r = new Value()
    NativeFunction(fun _ (y : Value) ->
        let r' = new Value()
        let x' = x.GetValue()
        let y' = y.GetValue()
        (match (x', y') with
        | (Double(x''), Double(y'')) ->
            Bool(Double.op_Equality(x'', y''))
        | _ -> failwith "dont match!") 
        |> r'.SetValue) 
    |> r.SetValue

let add _ (x : Value) : Value =
    let r = new Value()
    NativeFunction(fun _ (y : Value) ->
        let r' = new Value()
        let x' = x.GetValue()
        let y' = y.GetValue()
        (match (x', y') with
        | (Double(x''), Double(y'')) ->
            Double(x'' + y'')
        | _ -> failwith "Number!") 
        |> r'.SetValue) 
    |> r.SetValue

let subt _ (x : Value) : Value =
    let r = new Value()
    NativeFunction(fun _ (y : Value) ->
        let r' = new Value()
        let x' = x.GetValue()
        let y' = y.GetValue()
        (match (x', y') with
        | (Double(x''), Double(y'')) ->
            Double(x'' - y'')
        | _ -> failwith "Number!") 
        |> r'.SetValue) 
    |> r.SetValue

let mult _ (x : Value) : Value =
    let r = new Value()
    NativeFunction(fun _ (y : Value) ->
        let r' = new Value()
        let x' = x.GetValue()
        let y' = y.GetValue()
        (match (x', y') with
        | (Double(x''), Double(y'')) ->
            Double(x'' * y'')
        | _ -> failwith "Number!") 
        |> r'.SetValue) 
    |> r.SetValue

let print (e : Env) (x : Value) : Value =
    let r = new Value()        
    let streamvalue = e.getbinding "OUT"
    let streamraw = streamvalue.GetValue()
    match streamraw with
    | Any(stream') ->
        let stream = stream' :?> TextWriter
        (let x = x.GetValue() in
        match x with
        Str(s) -> 
            String.Format("{0}{1}", s, Environment.NewLine)
        | Double(d) -> 
            String.Format("{0}{1}", d, Environment.NewLine)
        | _ -> failwith "Error on print, unexpected argument") |> outstring stream
    | _ -> failwith "OUT must be a stream!"
    Unit |> r.SetValue

let sleep _ (x : Value) : Value =
    let r = new Value()    
    // waits here
    match x.GetValue() with
    | Double(d) -> 
        //Console.WriteLine("Thread.Sleep BEGIN: {0}", Thread.CurrentThread.ManagedThreadId)
        d |> Convert.ToInt32 |> Thread.Sleep
        //Console.WriteLine("Thread.Sleep END: {0}", Thread.CurrentThread.ManagedThreadId)
        Unit |> r.SetValue
    | _ -> failwith "Error on sleep"

let reduce _ (ls : Value) : Value =
    let r = new Value()
    NativeFunction(fun _ (init : Value) ->
        let r' = new Value()
        NativeFunction(fun e (fn : Value) ->
            let r'' = new Value()
            let ls' = ls.GetValue()
            (match ls' with
            | List(ls'') ->                 
                let tot = ref (init.GetValue())
                let fn' = fn.GetValue()
                for x in ls'' do
                    let partial = (Ev.Call e fn' !tot).GetValue()
                    tot := (Ev.Call e partial x).GetValue();
                !tot
            | _ -> failwith "1st arg must be a list!") |> r''.SetValue) 
        |> r'.SetValue) 
    |> r.SetValue

let greather_than _ (x : Value) : Value =
    let r = new Value()
    NativeFunction(fun _ (y : Value) -> 
        let r' = new Value()
        let x' = x.GetValue()
        let y' = y.GetValue()
        (match (x', y') with
        | (Double(vx), Double(vy)) ->
            Bool(vx > vy)
        | _ -> 
            failwith "Error on greather than") |> 
            r'.SetValue
    ) |> r.SetValue


type Ast = AstObject.Ast

and Compiled =
    val private tree : Ast
    val private env : Env
    
    new (src) = {        
        tree = Parser.Start Lexer.tokenstream (LexBuffer<char>.FromString src);
        env = new Env();
    }

    member this.DefaultBinds() =
        [("OUT", Any(Console.Out))
         ("write", NativeFunction(write))
         ("close", NativeFunction(close))
         ("==", NativeFunction(equals))
         ("print", NativeFunction(print))
         ("sleep", NativeFunction(sleep))
         (">", NativeFunction(greather_than))
         ("reduce", NativeFunction(reduce))
         ("+", NativeFunction(add))
         ("*", NativeFunction(mult))
         ("-", NativeFunction(subt))
         ("url", NativeFunction(url))
         ("http_server", NativeFunction(http_server))
         ("start", NativeFunction(start))
         ("stop", NativeFunction(stop))
         ("exit", NativeFunction(exit))
         ("get", NativeFunction(get_clr_property))
         ("set", NativeFunction(set_clr_property))
         //(":1+", NativeFunction(add_single_event_listener))
         ]

    member this.AddDefaultBinds() =
        for (key, value) in this.DefaultBinds() do
            value |> Value.CreateAndWrap |> this.env.addbinding key |> ignore;

    member this.AddBinding key (value : obj) =
        Any(value) |> Value.CreateAndWrap |> this.env.addbinding key |> ignore

    member this.Eval () =
        Ev.Eval this.env this.tree    

and Compiler =
    static member Compile (src) =
        let x = new Compiled(src)
        x

and Coelophysis =

    static member Compile (code : String) : Compiled =
        let compiled = Compiler.Compile code
        compiled.AddDefaultBinds()
        compiled
        
    static member Compile (stream : Stream) : Compiled =
        let reader = new StreamReader(stream)
        let code = reader.ReadToEnd()
        Coelophysis.Compile code

and CoelophysisExec =
    
    new () = {}

    static member Execute (args : String[]) : int =
        let manager = new AppDomainManager()
        let setup = new AppDomainSetup()
        let evid = new Evidence()
        let domain = manager.CreateDomain("Coelophysis", evid, setup)
        let asm = Assembly.GetCallingAssembly()
        domain.UnhandledException.AddHandler(fun _ e ->        
            raise (e.ExceptionObject :?> Exception))
        domain.ExecuteAssembly(asm.Location, args)

[<EntryPoint>]
let main argv = 
    let code = argv.[1]
    let compiled = Coelophysis.Compile code
    let r = compiled.Eval ()
    r.WaitForValue() |> ignore
    //printfn "END, count: %d, result: %A" (ThreadId.getcount()) (r.GetValue()) |> ignore
    Console.ReadLine()  |> ignore  
    0
