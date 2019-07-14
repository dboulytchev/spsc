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
      parse:
        x:LIDENT {Var x}
      | <c, a> : ctor[parse] {Ctor (c, a)}
      | f:LIDENT "(" args:!(Ostap.Util.list)[parse] ")" {Call (f, args)}
    )
      
  end
  
module Definition =
  struct
                                
    @type t =
      F of string * string list * Expr.t
    | G of string * Pattern.t * string list * Expr.t  with show, html

    let parse = ostap (
    f:LIDENT def:("(" p:!(Pattern.parse) args:(-"," !(Ostap.Util.list)[ostap (LIDENT)])? ")" {fun b -> G (f, p, (match args with None -> [] | Some args -> args), b)} |
                  args:(-"(" !(Ostap.Util.list)[ostap (LIDENT)] -")")? {fun b -> F (f, (match args with None -> [] | Some args -> args), b)}
                 ) "=" b:!(Expr.parse) ";" {def b}
    )    
    
  end

@type t = Definition.t list * Expr.t with show, html

let parse = ostap (
  defs:!(Definition.parse)* e:!(Expr.parse) {defs, e}
)

module S = Set.Make (String)

