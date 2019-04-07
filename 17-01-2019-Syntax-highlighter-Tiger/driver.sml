use "token.sml";
use "ishan.lex.sml";

val instream = TextIO.openIn "t1.tig";
val lexer = Mlex.makeLexer (fn _ => TextIO.input instream);

fun printOutput () = let val retval = lexer()
                     in (
                            if retval = "EOF" then ()
                            else (TextIO.print retval; printOutput ())
                      )
                    end;

val _ = printOutput ();