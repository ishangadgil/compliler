val inString = ref 0;
val prevLineNum = ref 0;
val lineNum = ref 1;
val prevCharNum = ref 0;
val charNum = ref 1;
val commentCnt = ref 0;

type lexresult = Tokens.token;
fun eof () = if !commentCnt > 0 then
			 (
			 	print("Improper Comments\n");
			 	Tokens.EOF(0, 0)
			 )
			 else
			 if !inString > 0 then
			 (
			 	print("Improper String\n");
			 	Tokens.EOF(0, 0)
			 )
			 else
			 (
			 	print ("\n");
			 	Tokens.EOF(0, 0)			 	
			 )
%%

alpha = [a-zA-Z];
digits = [0-9];
whitespace = [\t\ ];
%s COMMENT;
%s STRING;
%s MULTILINE_STRING;

%%

<INITIAL> \n 			=> (prevLineNum := !lineNum;
							prevCharNum := !charNum;
							lineNum := !lineNum + 1;
							charNum := 1;
							Tokens.WHITESPACE (yytext, !prevLineNum, !prevCharNum));
<INITIAL> {whitespace}+ => (prevCharNum := !charNum;
							charNum := !charNum + size	yytext;
							Tokens.WHITESPACE (yytext, !lineNum, !prevCharNum));
<INITIAL> "/*"			=> (YYBEGIN COMMENT;	
							prevCharNum := !charNum;
							commentCnt := !commentCnt + 1;
							prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.COMMENT (yytext, !lineNum, !prevCharNum));
<COMMENT> "/*"			=> (commentCnt := !commentCnt + 1;
							prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.COMMENT (yytext, !lineNum, !prevCharNum));
<COMMENT> "*/"			=> (commentCnt := !commentCnt - 1;
							if !commentCnt = 0 then (YYBEGIN INITIAL) else ();
							prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.COMMENT (yytext, !lineNum, !prevCharNum));
<COMMENT> \n 			=> (prevLineNum := !lineNum;
							prevCharNum := !charNum;
							lineNum := !lineNum + 1;
							charNum := 1;
							Tokens.COMMENT (yytext, !prevLineNum, !prevCharNum));
<COMMENT> [*/]			=> (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.COMMENT (yytext, !lineNum, !prevCharNum));
<COMMENT> [^*\n/]+ 		=> (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.COMMENT (yytext, !lineNum, !prevCharNum));
<INITIAL> \"				=> (YYBEGIN STRING;
							prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.STRING (yytext, !lineNum, !prevCharNum));
<STRING> [^\\"\n]+		=> (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.STRING (yytext, !lineNum, !prevCharNum));
<STRING> \\ (n | t | \^{alpha} | {digits}{digits}{digits} | \" | \\)
						=> (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.ESCAPE (yytext, !lineNum, !prevCharNum));	
<STRING> \\{whitespace}*\n
						=> (YYBEGIN MULTILINE_STRING;
							prevLineNum := !lineNum;
							prevCharNum := !charNum;
							lineNum := !lineNum + 1;
							charNum := 1;
							Tokens.ESCAPE (yytext, !prevLineNum, !prevCharNum));
<MULTILINE_STRING> {whitespace}*\n 	
						=> (prevLineNum := !lineNum;
							prevCharNum := !charNum;
							charNum := 1;
							Tokens.ESCAPE (yytext, !prevLineNum, !prevCharNum));
<MULTILINE_STRING> {whitespace}*\\
						=> (YYBEGIN STRING;
							prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.ESCAPE (yytext, !lineNum, !prevCharNum));
<STRING> \" 			=> (YYBEGIN INITIAL;
							inString := 0;
							prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.STRING (yytext, !lineNum, !prevCharNum));
<STRING> \n 			=> (YYBEGIN INITIAL;
							inString := 1;
							prevLineNum := !lineNum;
							prevCharNum := !charNum;
							lineNum := !lineNum + 1;
							charNum := 1;
							Tokens.STRING(yytext, !prevLineNum, !prevCharNum));
<INITIAL> array | break | do | else | end | for | function | if | in | let | nil | of | then | to | type | var | while 			
						=> (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.KEYWORD (yytext, !lineNum, !prevCharNum));
<INITIAL> "<>" | "<=" | ">=" | ":=" | "(" | ")" | "{" | "}" | "[" | "]" | "-"
						=> (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.OPERATOR (yytext, !lineNum, !prevCharNum));
<INITIAL> [:.,;*/+=<>&|] => (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.OPERATOR (yytext, !lineNum, !prevCharNum));
<INITIAL> {digits}+ 		=> (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.INT (valOf (Int.fromString yytext), !lineNum, !prevCharNum));
<INITIAL> {alpha}[a-zA-Z0-9_]*
						=> (prevCharNum := !charNum;
							charNum := !charNum + size yytext;
							Tokens.ID (yytext, !lineNum, !prevCharNum));
.						=> (print("Illegal Character"); continue());
							