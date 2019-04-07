use "token.sml";
use "lexer.lex.sml";

val instream = TextIO.openIn "testfile2.tig";
val lexer = Mlex.makeLexer (fn _ => TextIO.input instream);

fun printColoured (Tokens.COMMENT (t, _, _))     = (TextIO.print ("\027[1;31m" ^ t ^ "\027[0m"); true) |
    printColoured (Tokens.OPERATOR (t, _, _))    = (TextIO.print ("\027[1;32m" ^ t ^ "\027[0m"); true) |
    printColoured (Tokens.KEYWORD (t, _, _))     = (TextIO.print ("\027[1;33m" ^ t ^ "\027[0m"); true) |
    printColoured (Tokens.ID (t, _, _))          = (TextIO.print ("\027[1;34m" ^ t ^ "\027[0m"); true) |
    printColoured (Tokens.INT (t, _, _))         = (TextIO.print ("\027[1;35m" ^ (Int.toString t) ^ "\027[0m"); true) |
    printColoured (Tokens.STRING (t, _, _))      = (TextIO.print ("\027[1;36m" ^ t ^ "\027[0m"); true) |
    printColoured (Tokens.ESCAPE (t, _, _))      = (TextIO.print ("\027[1;94m" ^ t ^ "\027[0m"); true) |
    printColoured (Tokens.WHITESPACE (t, _, _))  = (TextIO.print ("\027[1;91m" ^ t ^ "\027[0m"); true) |
    printColoured (Tokens.EOF (_, _))            = false;

fun printOutput () = let val retval : Tokens.token = lexer()
                     in if (printColoured retval) then printOutput() else () end;

val _ = printOutput ();
