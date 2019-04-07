type lexresult = Tokens.token
fun eof () = Tokens.EOF (0,0)

%%
digit = [0-9];
whitespace = [\ \r\t\n];
alphabet = [A-Za-z];
symbols = ","| ":"| ";"| "("| ")"| "["| "]"| "{"| "}"| "."| "+"|"-"| "*"| "/"| "="| "<>"| "<"| "<="| ">"| ">="| "&"| "|"|":=";
%%

[\ \r\t\n] => (Tokens.WHITESPACE (yytext));
"let" => (Tokens.LET ());
"in" => (Tokens.IN ());
"end" => (Tokens.END ());
"type" => (Tokens.TYPE ());
"array" => (Tokens.ARRAY ());
"int" => (Tokens.INT ());
"=" => (Tokens.EQUAL ());
":" => (Tokens.COLON ());
"var" => (Tokens.VAR ());
{alphabet}+ => (Tokens.ALPHA (yytext));
{digit}+ => (Tokens.NUM (yytext));
\/\*(.*)\*\/ => (Tokens.COMMENT (yytext));
{symbols} => (Tokens.SYMBOL (yytext));
. => (TextIO.print ("ERROR"); continue());