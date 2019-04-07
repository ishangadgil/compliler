structure Tokens = 
struct 
	type token = string
	fun WHITESPACE (x) = x
	fun ALPHA (x) = x
	fun NUM (x) = x
	fun LET () = "\027[1;31mlet\027[0m"
	fun IN () = "\027[1;31min\027[0m"
	fun END () = "\027[1;31mend\027[0m"
	fun TYPE () = "\027[1;34mtype\027[0m"
	fun ARRAY () = "\027[1;33marray\027[0m"
	fun INT () = "\027[1;34mint\027[0m"
	fun EQUAL () = "="
	fun COLON () = ":"
	fun VAR () = "\027[1;34mvar\027[0m"
	fun COMMENT (x) = "\027[1;36m"^x^"\027[0m" 
	fun EOF   (x, y) = "EOF"
	fun SYMBOL (x) =  x
end


