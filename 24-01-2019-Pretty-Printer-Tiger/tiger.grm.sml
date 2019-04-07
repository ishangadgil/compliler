functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\007\000\009\000\006\000\012\000\005\000\013\000\004\000\000\000\
\\001\000\003\000\012\000\005\000\011\000\006\000\010\000\007\000\009\000\
\\008\000\008\000\010\000\021\000\000\000\
\\001\000\003\000\012\000\005\000\011\000\006\000\010\000\007\000\009\000\
\\008\000\008\000\011\000\025\000\000\000\
\\001\000\003\000\012\000\005\000\011\000\006\000\010\000\007\000\009\000\
\\008\000\008\000\014\000\020\000\000\000\
\\001\000\003\000\012\000\005\000\011\000\006\000\010\000\007\000\009\000\
\\008\000\008\000\015\000\024\000\000\000\
\\028\000\003\000\012\000\005\000\011\000\006\000\010\000\007\000\009\000\
\\008\000\008\000\000\000\
\\029\000\000\000\
\\030\000\000\000\
\\031\000\006\000\010\000\007\000\009\000\008\000\008\000\000\000\
\\032\000\006\000\010\000\007\000\009\000\008\000\008\000\000\000\
\\033\000\008\000\008\000\000\000\
\\034\000\008\000\008\000\000\000\
\\035\000\003\000\012\000\005\000\011\000\006\000\010\000\007\000\009\000\
\\008\000\008\000\000\000\
\\036\000\000\000\
\\037\000\003\000\012\000\005\000\011\000\006\000\010\000\007\000\009\000\
\\008\000\008\000\000\000\
\"
val actionRowNumbers =
"\001\000\006\000\001\000\008\000\
\\001\000\007\000\001\000\001\000\
\\001\000\001\000\001\000\004\000\
\\002\000\013\000\012\000\011\000\
\\010\000\009\000\001\000\001\000\
\\005\000\003\000\014\000\001\000\
\\015\000\000\000"
val gotoT =
"\
\\001\000\001\000\003\000\025\000\000\000\
\\000\000\
\\001\000\011\000\000\000\
\\000\000\
\\001\000\012\000\000\000\
\\000\000\
\\001\000\013\000\000\000\
\\001\000\014\000\000\000\
\\001\000\015\000\000\000\
\\001\000\016\000\000\000\
\\001\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\020\000\000\000\
\\001\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\024\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 26
val numrules = 12
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | STRING of  (string)
 | INT of  (int) | program of  (Ast.exp) | explist of  (Ast.exp list)
 | exp of  (Ast.exp)
end
type svalue = MlyValue.svalue
type result = Ast.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "INT"
  | (T 2) => "PLUS"
  | (T 3) => "FOR"
  | (T 4) => "MINUS"
  | (T 5) => "MUL"
  | (T 6) => "DIV"
  | (T 7) => "EQ"
  | (T 8) => "IF"
  | (T 9) => "THEN"
  | (T 10) => "ELSE"
  | (T 11) => "STRING"
  | (T 12) => "LET"
  | (T 13) => "IN"
  | (T 14) => "END"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.program ( exp )
 in ( LrTable.NT 2, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.exp ( Ast.intExp INT     )
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.STRING STRING, (STRINGleft as STRING1left), 
STRING1right)) :: rest671)) => let val  result = MlyValue.exp (
Ast.stringExp (STRING, STRINGleft))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
PLUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
 Ast.opExp  {left=exp1, oper=Ast.plusOp, right= exp2, pos=PLUSleft} )
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
MINUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
 Ast.opExp  {left=exp1, oper=Ast.minOp, right= exp2, pos=MINUSleft} )
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
MULleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
 Ast.opExp  {left=exp1, oper=Ast.mulOp, right= exp2, pos=MULleft} )
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
DIVleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
 Ast.opExp  {left=exp1, oper=Ast.divOp, right= exp2, pos=DIVleft} )
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, EQleft
, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (
 Ast.opExp  {left=exp1, oper=Ast.eqOp, right= exp2, pos=EQleft} )
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp exp2, _, _))
 :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, (LETleft as 
LET1left), _)) :: rest671)) => let val  result = MlyValue.exp (
 Ast.letExp {decs= exp1, body= exp2, pos= LETleft})
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (
Ast.ifExp {cond=exp1, body1= exp2, body2= exp3, pos=IFleft})
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.explist (nil)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( MlyValue.explist explist, _, explist1right)) :: ( _,
 ( MlyValue.exp exp, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.explist ( exp :: explist         )
 in ( LrTable.NT 1, ( result, exp1left, explist1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.INT i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.STRING i,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
end
end
