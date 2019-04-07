signature Tiger_TOKENS =
sig
type ('a,'b) token
type svalue
val END:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Tiger_LRVALS=
sig
structure Tokens : Tiger_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
