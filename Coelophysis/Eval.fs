module Eval

open AstObject

type Ev =
    // when evaling user functions
    // we can't just eval the body with a new env. otherwise we won't have
    // our vars in a nested function. 
    // we need to preserve the env we create, maybe we should
    // use a combinator to wrap the result in a new function
    // that binds the argument to the eval passed, instead of replacing it
    // and returning the body.
    static member Call (e : Env) (left : Ast) (right : Ast) = 
        let application' = new Value()
        (fun _ -> 
            let aright' = Ev.Eval e right
            let left' = (Ev.Eval e left).GetValue()
            match left' with
            | NativeFunction(f) ->                     
                let r = f e aright'
                r.GetValue()
            | _ as c -> failwith "Error!!") |> application'.SetValue

    // eval the x ast with an environment e
    static member Eval (e : Env) (x : Ast) : Value =  
        match x with
        | IfThenElse(cond, truebr, falsebr) ->
            let ifres' = new Value()
            (fun _ ->
                let cond' = Ev.Eval e cond
                let cond'' = cond'.GetValue()
                let toeval = (match cond'' with
                                | Bool(true) -> 
                                    truebr
                                | Bool(false) -> 
                                    falsebr
                                | _ -> failwith "Condition doesnt return bool!")
                let toeval' = Ev.Eval e toeval 
                toeval'.GetValue()) |> ifres'.SetValue 
        | Set(Identifier(i), expr) ->
            let set' = new Value()
            (fun _ ->            
                e.addbinding i set' |> ignore
                let expr' = Ev.Eval e expr
                expr'.GetValue()) |> set'.SetValue
        | Pipe(hd, tl) ->
            // can this be encoded as an application? NOPE
            //Application(Function(Identifier("_"), tl), hd) |> Eval e
            // semantics:
            // * the result of a pipe is the result of its tail
            // * functions are non-strict. pipes should be strict
            //   this means that head must not fail for us to compute tail
            let pipe' = new Value()             
            (fun _ ->
                let hd' = Ev.Eval e hd
                // we can have side effects on head, 
                // so we need to wait for whatever it does
                // before we continue ...
                hd'.WaitForValue() |> ignore
                let e' = e.duplicate()         
                e'.addbinding "_" hd' |> ignore
                let tl' = Ev.Eval e' tl
                tl'.GetValue()) |> pipe'.SetValue
        | Fork(ls) ->
            let fork' = new Value()
            (fun _ -> 
                let evaled = List.map (Ev.Eval e) ls
                // Wait for all to end!
                List.iter (fun (r : Value) -> r.WaitForValue() |> ignore) evaled
                let r = evaled |> List.rev |> List.head
                r.GetValue()) |> fork'.SetValue
        | Application(left, right) -> 
            Ev.Call e left right
        | Function(Identifier(i), body) ->            
            NativeFunction(fun env arg -> 
                let r = new Value()
                let env2 = e.duplicate()
                let er = Ev.Eval (env2.addbinding i arg) body
                er.GetValue() |> r.SetValue
                ) |> Value.CreateAndWrap
        | Identifier(i) -> e.getbinding i
        | _ when IsValue(x) -> x |> Value.CreateAndWrap
        | _ as c -> failwith "Error!"
