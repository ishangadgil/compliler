structure Tokens = struct
	datatype token = COMMENT of string * int * int
				   | EOF of int * int
				   | OPERATOR of string * int * int
				   | KEYWORD of string * int * int
				   | ID of string * int * int
				   | INT of int * int * int 
				   | STRING of string * int * int
				   | ESCAPE of string * int * int
				   | WHITESPACE of string * int * int;
end;