open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram = Semantic.check program in
  print_endline (string_of_sprogram sprogram)
