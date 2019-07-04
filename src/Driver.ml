open GT
open Ostap

let parse infile =
  let s   = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.lident [] s
       inherit Util.Lexers.uident [] s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip.nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (!(Ast.parse) -EOF))

let main =
  try
    let infile = Sys.argv.(1) in    
    match parse infile with
    | `Ok ((defs, expr) as prog) -> Printf.printf "%s\n" (Eval.show_value @@ Eval.eval (new Eval.env prog) expr)
    | `Fail er -> Printf.eprintf "Error: %s\n" er
  with Invalid_argument _ ->
    Printf.printf "Usage: spsc <input file.sll>\n"
