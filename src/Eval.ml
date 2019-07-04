open GT
open Ast

type 'env value = string * ('env * Expr.t) list 
                 
let rec eval env = function
| Expr.Var   x         -> let env', e = env#lookup x in eval env' e
| Expr.Ctor (c, args)  -> (c, List.map (fun a -> env, a) args)
| Expr.Call (f, (x::xs as args)) ->
   let v = eval env x in
   (match env#lookupDef f v with
    | `F (fargs, body)       -> eval (env#bind  fargs (List.map (fun a -> env, a) args)) body
    | `G (fargs, body, env') -> eval (env'#bind fargs (List.map (fun a -> env', a) xs  )) body      
   )

let show_value v =
  let buf = Buffer.create 64 in
  let puts s = Buffer.add_string buf s in
  let rec inner (ctor, args) =
    let len = List.length args in
    puts ctor;
    (fun args -> if len > 0 then (puts " ("; args (); puts ")") else args ())
    (fun _ -> List.iteri (fun i (env, e) -> if i > 0 then puts ", "; inner (eval env e)) args)
  in
  inner v;
  Buffer.contents buf

class env ((defs, _) : Ast.t) = 
object (self : 'self)
     
  val vars = []
           
  method lookupDef f (ctor, args) =
    try 
      ((match 
        List.find
          (function
           | Definition.F (f', _, _) when f' = f -> true
           | Definition.G (f', (ctor', _), _, _) when f' = f && ctor' = ctor -> true
           | _ -> false
          )
          defs
       with
       | Definition.F (_, fargs, body)             -> `F (fargs, body)
       | Definition.G (_, (_, pargs), fargs, body) -> `G (fargs, body, self#bind pargs args)) :>
        [`F of _ | `G of _])
    with Not_found ->
      invalid_arg (Printf.sprintf "Undefined function '%s' (or undefined case '%s')" f ctor) 
                                  
  method lookup x =
    try List.assoc x vars with Not_found ->
      try
        let Definition.F (_, _, e) = List.find (function Definition.F (f, [], _) when f = x -> true | _ -> false) defs in
        ({< vars = [] >}, e) 
      with Not_found -> invalid_arg (Printf.sprintf "Undefined name %s" x)
                      
  method bind fargs args =
    if List.length fargs <> List.length args
    then invalid_arg "Wrong number of arguments"
    else {< vars = List.combine fargs args @ vars >}

  method show =
    "<env>"
    
end

  
  
               
