structure Parse : sig 
	val parse : string -> unit  
	val run_test : int -> unit
	val run_tests : int -> unit
end =
struct 
  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
  structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
  structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
  fun parse filename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
	  val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
       in TextIO.closeIn file;
	   absyn
      end handle LrParser.ParseError => raise ErrorMsg.Error

	fun run_test test = parse ("../testcases/test" ^ Int.toString test ^ ".tig")
	fun run_tests highest = 
		let
      fun loop i =
        if i > highest then ()
        else
          (run_test i; loop (i + 1))
    in
      loop 1
    end
end



