structure Semant =
struct
	type venv = Env.enventry Symbol.table
	type tenv = Types.ty Symbol.table

	type expty = {exp: Translate.exp, ty: Types.ty}

	structure A = Absyn
	structure T = Types
	structure Err = ErroMsg

	fun checkInt ({exp=_,ty=T.INT},pos) = ()
			  |	 ({exp=_,ty=_},pos) = ErrorMsg.error pos "Error : Integer required!"

    fun checkEqualityOp ({exp=_, ty=T.INT}, {exp=_, ty=T.INT}, pos) = ()
  	  | checkEqualityOp ({exp=_, ty=T.STRING}, {exp=_, ty=T.STRING}, pos) = ()
  	  | checkEqualityOp ({exp=_, ty=T.RECORD(_, ref1)}, {exp=_, ty=T.RECORD(_, ref2)}, pos) = if ref1 = ref2 then () else Err.error pos "Error: Can't compare different record types!"
  	  | checkEqualityOp ({exp=_, ty=T.NIL}, {exp=_, ty=T.RECORD(_, _)}, pos) = ()
  	  | checkEqualityOp ({exp=_, ty=T.RECORD(_, _)}, {exp=_, ty=T.NIL}, pos) = ()
  	  | checkEqualityOp ({exp=_, ty=T.ARRAY(_, ref1)}, {exp=_, ty=T.ARRAY(_, ref2)}, pos) = if ref1 = ref2 then () else Err.error pos "Error: Can't compare different array types!"
  	  | checkEqualityOp ({exp=_, ty=_}, {exp=_, ty=_}, pos) = Err.error pos "error : comparison expected both int, string, record, or array"

	fun checkComparisonOp ({exp=_, ty=T.INT}, {exp=_, ty=T.INT}, pos) = ()
	  | checkComparisonOp ({exp=_, ty=T.STRING}, {exp=_, ty=T.STRING}, pos) = ()
	  | checkComparisonOp ({exp=_, ty=_ }, {exp=_, ty=_ }, pos) = Err.error pos "Error: Comparison of incompatible types!"



	fun transExp(venv,tenv,exp) = 
		let fun trexp (A.VarExp(var)) = trvar var
			  |	transExp (A.NilExp) = {exp=(), ty=T.NIL}
			  | trexp (A.IntExp(intvalue)) = {exp=(), ty=T.INT}
			  | trexp (A.StringExp(stringvalue, pos)) = {exp=(), ty=T.STRING}
			  | trexp (A.SeqExp(expList)) = 
                let
                  fun seqHelp((seqExp, pos), {exp=_, ty=_}) = (trexp seqExp)
                  fun checkSequence sequence = foldl seqHelp {exp=(), ty=T.UNIT} sequence
                in
                  checkSequence expList
                end 

			  | trexp (A.OpExp{left,oper, right, pos}) = 
														(case oper of 

															A.PlusOp => (checkInt(trexp left, pos);
															 			checkInt(trexp right, pos);
															 			{exp = (), ty=Types.INT})
														|	A.MinusOp => (checkInt(trexp left, pos);
																		  checkInt(trexp right, pos);
																		  {exp = (), ty=Types.INT})
														|	A.TimesOp => (checkInt(trexp left, pos);
															 			checkInt(trexp right, pos);
															 			{exp = (), ty=Types.INT})
														|	A.DivideOp => (checkInt(trexp left, pos);
																		  checkInt(trexp right, pos);
																		  {exp = (), ty=Types.INT})
														|	A.EqOp => (checkEqualityOp(trexp left, pos);
															 			checkInt(trexp right, pos);
															 			{exp = (), ty=Types.INT})
														|	A.NeqOp => (checkEqualityOp(trexp left, pos);
																		  checkInt(trexp right, pos);
																		  {exp = (), ty=Types.INT})
														|	A.LtOp => (checkComparisonOp(trexp left, pos);
															 			checkInt(trexp right, pos);
															 			{exp = (), ty=Types.INT})
														|	A.LeOp => (checkComparisonOp(trexp left, pos);
																		  checkInt(trexp right, pos);
																		  {exp = (), ty=Types.INT})
														|	A.GtOp => (checkComparisonOp(trexp left, pos);
															 			checkInt(trexp right, pos);
															 			{exp = (), ty=Types.INT})
														|	A.GeOp => (checkComparisonOp(trexp left, pos);
																		  checkInt(trexp right, pos);
																		  {exp = (), ty=Types.INT})
														)
		
			   
		      and trvar (A.SimpleVar(id,pos)) = 
		      			(case Symbol.look(venv,id) of
		      				SOME(Env.VarEntry{ty}) => ({exp(), ty = actual_ty ty})

		      			|	NONE => (error pos ("undefined variable "^S.name id);
		      						 {exp = (), ty = Types.INT})
		      			)
	     in
	     	()
	     end

end