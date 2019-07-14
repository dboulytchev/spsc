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
    let mode   = Sys.argv.(1) in
    let infile = Sys.argv.(2) in    
    match parse infile with
    | `Ok ((defs, expr) as prog) ->
       (match mode with
        | "-s" ->
           let f = open_out (infile ^ ".dot") in
           Printf.fprintf f "%s\n" (Driving.to_graph (Driving.drive prog));
           close_out f;
           ignore @@ Sys.command ("dot -Tpng " ^ infile ^ ".dot" ^ " -o " ^ infile ^ ".png");
           ignore @@ Sys.command ("eog " ^ infile ^ ".png")
          
        | "-i" -> Printf.printf "%s\n" (Eval.show_value @@ Eval.eval (new Eval.env prog) expr)
       )
    | `Fail er -> Printf.eprintf "Error: %s\n" er
  with Invalid_argument _ ->
    Printf.printf "Usage: spsc <input file.sll>\n"
