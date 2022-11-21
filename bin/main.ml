let () =
  let lexbuf = Lexing.from_channel stdin in
  let program = Panda.Parser.program Panda.Scanner.token lexbuf in
  let sprogram = Panda.Semantic.check program in
  print_endline (Panda.Sast.string_of_sprogram sprogram)


