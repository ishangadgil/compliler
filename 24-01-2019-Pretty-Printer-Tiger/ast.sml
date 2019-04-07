structure Ast = 
struct
	type pos = int
	datatype exp = intExp of int
				 | stringExp of string*pos
				 | expList of exp list
				 | ifExp of {cond:exp, body1:exp, body2:exp, pos:pos}
				 | letExp of {decs:exp, body:exp, pos:pos}
				 | opExp of {left:exp, oper:oper, right:exp, pos:pos}

		and oper = eqOp
				 | plusOp
				 | minOp
				 | mulOp
				 | divOp
end