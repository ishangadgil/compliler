structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val eof = fn () => Tokens.EOF(0,0)

%%
%header (functor TigerLexFun (structure Tokens: Tiger_TOKENS));
digit=[0-9];
alpha=[A-Za-z];
%%

[\ \r\t\n] => (lex());
"+"        => (Tokens.PLUS   (yypos, yypos+1));
"-"        => (Tokens.MINUS   (yypos, yypos+1));
"*"        => (Tokens.MUL   (yypos, yypos+1));
"/"        => (Tokens.DIV   (yypos, yypos+1));
"="        => (Tokens.EQ  (yypos, yypos+1));
"let"      => (Tokens.LET    (yypos, yypos+3));
"in"       => (Tokens.IN     (yypos, yypos+2));
"end"      => (Tokens.END    (yypos, yypos+3));
"for"      => (Tokens.FOR    (yypos, yypos+3));
"if"       => (Tokens.IF     (yypos, yypos+2));
"then"     => (Tokens.THEN   (yypos, yypos+4));
"else"     => (Tokens.ELSE   (yypos, yypos+4));
{alpha}+   => (Tokens.STRING (yytext, yypos, yypos+(String.size yytext)));
{digit}+   => (let val SOME x = Int.fromString yytext
		in 
			Tokens.INT (x, yypos, yypos + (String.size yytext))
		end);
.          => (ErrorMsg.error yypos ("illegal character"^yytext); continue());
