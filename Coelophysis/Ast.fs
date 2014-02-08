module AstObject

open System
open System.Threading

// a binding between a varname and a value
// this was made like this to support global state
type Binding = 
    val varName : string
    val mutable varValue : Value
    new(varName, varValue) = { varName = varName; varValue = varValue; }

// environment: a list of bindings
and Env = 
    val mutable bindings : List<Binding>

    new() = {  
        bindings = List.empty
    }

    new(bindings) = { bindings = bindings }
    
    member this.duplicate () : Env =
        let x = new Env(List.map id this.bindings)
        x

    member this.addbinding varname varvalue =         
        this.bindings <- new Binding(varname, varvalue) :: this.bindings
        this

    member this.getbinding varname = 
        let rec getbindingaux varname (ls : List<Binding>)=      
            if ls.IsEmpty then
                NoVarFound |> Value.CreateAndWrap
            else
                let b = ls.Head in
                if b.varName.Equals(varname) then
                    b.varValue
                else
                    getbindingaux varname ls.Tail
        // --
        getbindingaux varname this.bindings

and ThreadId() =
    static let mutable value : int64  = 0L
    static let mutable count : int64  = 0L

    static member getid () =
        Interlocked.Increment(&value) |> ignore
        Interlocked.Increment(&count)

    static member dec() =
        Interlocked.Decrement(&count)

    static member getcount() =
        count

and NativeFunctionType = Env -> Value -> Value
and Ast = Str of string
         | Symbol of string
         | Any of obj
         | List of List<Ast>
         | Char of char
         | Double of double
         | Bool of bool
         | Identifier of string
         | Pipe of Ast * Ast
         | Function of Ast * Ast
         | Application of Ast * Ast     
         | Set of Ast * Ast
         | IfThenElse of Ast * Ast * Ast
         | NativeFunction of NativeFunctionType
         | Fork of List<Ast>
         | Unit
         | NoVarFound 
and Value = 
    val mutable private value : Option<Ast>
    val mutable private hasValue : bool
    val private wOne : ManualResetEvent
    
    new () = {
        value = None;        
        hasValue = false;
        wOne = new ManualResetEvent(false);                
    }

    member this.SetValue generator : Value =
        let dbgid = ThreadId.getid()

        //Console.WriteLine("[{0}] Thread {1} queues SetValue", dbgid, Thread.CurrentThread.ManagedThreadId)

        ThreadPool.QueueUserWorkItem(fun x -> 
            //Console.WriteLine("[{0}] Thread {1} queued SetValue executing...", dbgid, Thread.CurrentThread.ManagedThreadId)            
            generator x |> this.SetValue |> ignore
            //Console.WriteLine("[{0}] Thread {1} queued SetValue executed!", dbgid, Thread.CurrentThread.ManagedThreadId)   
            
            ThreadId.dec() |> ignore
            ()) |> ignore
        this

    member this.SetValue (v : Ast) : Value =
        if this.hasValue then
            failwith "Already has value!"
        else
            this.value <- Some v
            this.hasValue <- true
            this.wOne.Set() |> ignore
            this

    member private this.ValueOrFail () : Ast =
        match this.value with
        | Some(v) -> v
        | _ -> failwith "No value!"

    member this.WaitForValue () : Value =        
        this.wOne.WaitOne() |> ignore
        this

    member this.WaitForValue callback : Value =
        let dbgid = ThreadId.getid()

        //Console.WriteLine("[{0}] Thread {1} queues WaitForValue", dbgid, Thread.CurrentThread.ManagedThreadId)

        ThreadPool.QueueUserWorkItem(fun state ->
            // wait here
            //Console.WriteLine("[{0}] Thread {1} queued WaitForValue executing...", dbgid, Thread.CurrentThread.ManagedThreadId)
            this.WaitForValue () |> ignore
            //Console.WriteLine("[{0}] Thread {1} queued WaitForValue executed!", dbgid, Thread.CurrentThread.ManagedThreadId)
            callback this            

            ThreadId.dec() |> ignore
            ()) |> ignore
        this

    member this.GetValue() : Ast =
        // wait here
        this.WaitForValue () |> ignore
        let value = this.ValueOrFail()
        value

    member this.GetValue callback : Value =
        let dbgid = ThreadId.getid()

        //Console.WriteLine("[{0}] Thread {1} queues GetValue", dbgid, Thread.CurrentThread.ManagedThreadId)

        ThreadPool.QueueUserWorkItem(fun state ->   
            //Console.WriteLine("[{0}] Thread {1} queued GetValue executing...", dbgid, Thread.CurrentThread.ManagedThreadId)
            let value = this.GetValue()
            //Console.WriteLine("[{0}] Thread {1} queued GetValue executed!", dbgid, Thread.CurrentThread.ManagedThreadId)
            callback value

            ThreadId.dec() |> ignore
            ()) |> ignore
        this        

    static member CreateAndWrap (v : Ast) =
        let x = new Value()
        v |> x.SetValue

    static member CreateAndWrap (v : obj -> Ast) =
        let x = new Value()
        v |> x.SetValue


let IsValue (x : Ast) = 
    match x with
    Str(_) | Char(_) | Double(_) | Any(_) | Symbol(_)
    | Bool(_) | NativeFunction(_) | List(_) | Unit | NoVarFound -> true
    | _ -> false

let IsCallable (x : Ast) = 
    match x with
    NativeFunction(_) | Function(_, _) -> true
    | _ -> false
