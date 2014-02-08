module HttpServer

open System
open System.Net

open AstObject
open Eval

type OnContextDelegate = delegate of HttpListenerContext -> unit

type HttpServer() =
    let listener : HttpListener = new HttpListener()
    let mutable _on_context : Ast = Unit

    member this.on_context with get ()  = _on_context
                                and set(new_on_context) = _on_context <- new_on_context
        

    member this.GetContext (cb : IAsyncResult) =
        try
            let ctx = listener.EndGetContext(cb)
            if IsCallable(_on_context) then
                let e = new Env()
                Any(ctx) |> Ev.Call e _on_context |> ignore
            else
                ()
        with
        | _ as ex -> ()
                
        if listener.IsListening then
            try
                listener.BeginGetContext(new AsyncCallback(this.GetContext), this) |> ignore
            with
            | _ as ex -> ()
        else
            ()

    member this.Init ls =
        for x in ls do
            listener.Prefixes.Add x;
        this

    member this.Start () =    
        listener.Start()
        listener.BeginGetContext(new AsyncCallback(this.GetContext), this) |> ignore
        this

    member this.Stop () =
        listener.Stop()
        this