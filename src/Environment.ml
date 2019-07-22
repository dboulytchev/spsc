open Ast
   
class env ((defs, _) : t) = 
object (self : 'self)
     
  val vars = []

  method bound (x : string) = try ignore (List.assoc x vars); true with Not_found -> false
  
  method lookup x =
    try (List.assoc x vars : ('self * Expr.t))
    with Not_found -> invalid_arg (Printf.sprintf "Undefined name %s" x)
                      
  method bind fargs args =
    if List.length fargs <> List.length args
    then invalid_arg "Wrong number of arguments"
    else ({< vars = List.combine fargs args @ vars >} : 'self)

end

let couple env = List.map (fun a -> env, a) 
