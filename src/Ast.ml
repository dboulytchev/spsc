open GT
open Ostap
   
let ctor p =
  ostap (
    c:UIDENT args:(-"(" !(Ostap.Util.list)[p] -")")? {
       c, match args with None -> [] | Some args -> args
    }
  ) 
           
module Pattern =
  struct
    
    @type t = string * string list with show, html

    let parse = ostap (ctor[ostap (LIDENT)]) 
    
  end

module Expr =
  struct

    @type t =
    | Var  of string
    | Ctor of string * t list
    | Call of string * t list with show, html, foldl

    ostap (
      parse[var]:
        x:LIDENT {if var x then Var x else Call (x, [])}
      | <c, a> : ctor[parse var] {Ctor (c, a)}
      | f:LIDENT "(" args:!(Ostap.Util.list)[parse var] ")" {Call (f, args)}
    )
      
  end
  
module Definition =
  struct
                                
    @type t =
      F of string * string list * Expr.t
    | G of string * Pattern.t * string list * Expr.t  with show, html

    let parse =
      let opt_to_list = function None -> [] | Some l -> l in
      ostap (
        f:LIDENT
        <(def, args)> :("(" p:!(Pattern.parse) args:(-"," !(Ostap.Util.list)[ostap (LIDENT)])? ")" {let args = opt_to_list args in (fun b -> G (f, p, args, b)), args @ snd p} |
              args:(-"(" !(Ostap.Util.list)[ostap (LIDENT)] -")")? {let args = opt_to_list args in (fun b -> F (f, args, b)), args}
        ) "=" b:!(Expr.parse (fun x -> List.exists (fun y -> y = x) args)) ";" {def b}
    )    
    
  end

@type t = Definition.t list * Expr.t with show, html

let parse = ostap (
  defs:!(Definition.parse)* e:!(Expr.parse (fun x -> not @@ List.exists (function Definition.F (f, _, _) | Definition.G (f, _, _, _) -> f = x) defs)) {defs, e}
)

module S = Set.Make (String)

let fv expr =
  transform(Expr.t)
    (fun f ->
       object inherit [_, _] @Expr.t[foldl] f
         method c_Var vs _ x = S.add x vs
       end
    ) S.empty expr

let renames lower upper =
  let id x = x in
  let update s x y =
    let x' = s x in
    if x' = x then Some (fun z -> if z = x then y else s z) else None
  in
  let bind s f = match s with None -> None | Some x -> f x in
  let rec inner s = function
  | Expr.Var lx, Expr.Var ux ->
     if s lx = ux then Some s else update s lx ux
  | Expr.Ctor (lc, la), Expr.Ctor (tc, ta) 
  | Expr.Call (lc, la), Expr.Call (tc, ta) ->
     if lc = tc && List.length la = List.length ta
     then List.fold_left (fun s p -> bind s (fun s -> inner s p)) (Some s) @@List.combine la ta    
     else None
  | _ -> None
  in
  inner id (lower, upper)
       
