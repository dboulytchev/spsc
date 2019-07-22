open GT
open Ast

type 'env context = Top | G of string * Expr.t list * 'env * 'env context

type 'env conf = 'env * 'env context * Expr.t
               
type 'env node =
| Var    of string
| Ctor   of string * 'env node Lazy.t list
| Unfold of Expr.t Lazy.t * 'env node Lazy.t
| Assume of Expr.t Lazy.t * string * 'env node Lazy.t list
| Rename of Expr.t Lazy.t * 'env node 

let expand_conf (env, context, expr) =
  lazy (
    let rec expand_expr env = function
    | Expr.Var x as expr ->
       if env#bound x
       then let env', expr = env#lookup x in expand_expr env' expr
       else expr
    | Expr.Ctor (c, args) ->
       Expr.Ctor (c, List.map (expand_expr env) args)
    | Expr.Call (f, args) ->
       Expr.Call (f, List.map (expand_expr env) args)
    in      
    let rec unfold acc = function
    | Top -> acc ()
    | G (g, args, env, context') ->
       unfold (fun _ -> Expr.Call (g, acc () :: (List.map (expand_expr env) args))) context'
    in
    unfold (fun _ -> expand_expr env expr) context
  )

let fix f =
  let x = Pervasives.ref (Var "") in
  let m = f x in
  x := m;
  m

let rec renames lower = function
| [] -> None
| lnode :: stack ->
   match lnode with 
   | (Unfold (upper, _) | Assume (upper, _, _)) as node ->
      (match Ast.renames lower (Lazy.force upper) with
       | Some _ -> Some node
       | _      -> renames lower stack
      )
   | _ -> renames lower stack
    
let rec drive stack context (env, expr) =
  let conf = expand_conf (env, context, expr) in
  match renames (Lazy.force conf) stack with
  | Some node -> Rename (conf, node)
  | None ->
     (match expr with    
     | Expr.Var x ->
        if env#bound x
        then drive stack context (env#lookup x)
        else
          (match context with
           | Top -> Var x
           | G (g, args, env, context') ->
              fix (fun self ->
                Assume (conf, x, List.map (fun env -> lazy (drive (!self :: stack) (G (g, args, env, context')) (env, expr))) (env#assume g x))
              )
          )
       
     | Expr.Ctor (c, args) ->
        (match context with
         | Top -> Ctor (c, List.map (fun a -> lazy (drive stack Top (env, a))) args)
         | G (g, ys, env', context') ->
            let env'', fargs, body = env#lookupCase g c (Environment.couple env args) in
            fix (fun self ->
              Unfold (conf, lazy (drive (!self :: stack) context' (env''#bind fargs (Environment.couple env' ys), body)))
            )
        )

     | Expr.Call (f, args) ->
        (match env#lookupDef f with
         | `F (fargs, body) ->
            fix (fun self ->
              Unfold (conf, lazy (drive (!self :: stack) context (env#bind fargs (Environment.couple env args), body)))
            )
         
         | `G -> let x::xs = args in drive stack (G (f, xs, env, context)) (env, x)
        )
     )
     
class env ((defs, _) as ast : Ast.t) = 
object (self : 'self) inherit Environment.env ast
  val index = 0
  method lookupDef f =
    try
      ((match
        List.find
          (function
           | Definition.F (f', _, _) | Definition.G (f', _, _, _) -> f = f'
          )
          defs
      with
      | Definition.F (_, fargs, body) -> `F (fargs, body)
      | Definition.G  _               -> `G) :> [`F of _ | `G])
    with Not_found -> invalid_arg (Printf.sprintf "Undefined function '%s'" f)
                    
  method lookupCase g c args =
    try
      match
        List.find
          (function
           | Definition.G (g', (c', _), _, _) when g = g' && c = c' -> true
           | _ -> false
          )
          defs
      with
      | Definition.G (_, (_, cargs), fargs, body) -> self#bind cargs args, fargs, body
    with Not_found -> invalid_arg (Printf.sprintf "Undefined function '%s' or case '%s'" g c)
    
  method assume g x =
    List.map
      (function Definition.G (_, (ctor, cargs), _, _) ->
         let n    = List.length cargs in
         let vars = List.init n (fun i -> Expr.Var (Printf.sprintf "$%d" (index + i))) in
         let expr = Expr.Ctor (ctor, vars) in
         let env' = {< index = index + n >} in
         env'#bind [x] [(env', expr)]
      )
    @@
    List.filter (function Definition.G (g', _, _, _) when g = g' -> true | _ -> false) defs
end

let drive ((_, expr) as t) = drive [] Top (new env t, expr)

let to_graph root =
  let node_id node = (Obj.magic node : int) in
  let buf          = Buffer.create 1024 in
  let add s        = Buffer.add_string buf s in
  let rec inner node =    
    let current = string_of_int @@ node_id node in
    let add_edges ns =
      List.iter (fun n -> add "  "; add current; add " -> "; add @@ string_of_int @@ node_id (Lazy.force n); add ";\n") ns;
      List.iter (fun n -> inner (Lazy.force n)) ns
    in
    match node with
    | Var     x         -> add "  \""; add current; add "\" [label=\"Var ("; add x; add ")\"];\n"
    | Ctor   (c, ns)    -> add "  \""; add current; add "\" [label=\"Ctor ("; add c; add ")\"];\n"; add_edges ns
    | Unfold (e, n)     -> add "  \""; add current; add "\" [label=\"Unfold ("; add (String.escaped (show(Expr.t) @@ Lazy.force e)); add ")\"];\n"; add_edges [n]
    | Assume (e, x, ns) -> add "  \""; add current; add "\" [label=\"Assume ("; add (String.escaped (show(Expr.t) @@ Lazy.force e)); add ", "; add x; add ")\"];\n"; add_edges ns
    | Rename (e, n)     -> add "  \""; add current; add "\" [label=\"Rename ("; add (String.escaped (show(Expr.t) @@ Lazy.force e)); add ")\"];\n";
                           add "  "; add current; add " -> "; add @@ string_of_int @@ node_id n; add ";\n"
  in
  add "digraph g {\n";
  add "  node [shape=box];\n";
  inner root;
  add "}\n";
  Buffer.contents buf
  
