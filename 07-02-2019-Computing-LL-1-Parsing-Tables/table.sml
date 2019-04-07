type LLKey = Atom.atom * Atom.atom

structure LLTABLE_KEY : ORD_KEY = struct
	type ord_key = LLKey
	fun compare ((a,b), (c,d)) = 
		let 
			fun compareSecond () = 
			(
				if Atom.lexCompare (b, d) = GREATER then
					GREATER
				else if Atom.lexCompare (b, d) = EQUAL then
					EQUAL
				else 
					LESS
			)
		in
		(
			if Atom.lexCompare (a,c) = EQUAL then
				compareSecond ()
			else if Atom.lexCompare (a, c) = GREATER then
				GREATER
			else
				LESS
		)
		end
end

structure LLMap = RedBlackMapFn (LLTABLE_KEY)

type LLRules = Productions LLMap.map

val llTable:LLRules ref = ref LLMap.empty;

(*Initialise the table, the table is of the form (Symbol,Token) -> RHS *)

fun initialiseLLTable (x :: xs) = 
	let
		fun initLL (y :: z) = 
			let
				fun initLLTable (symbol, token) = 
					(llTable := LLMap.insert(!llTable, (symbol, token), RHSSet.empty))
			in
			(
				initLLTable(x , y);
				initLL z
			)
			end
			

		  | initLL [] = ()
	in
	(
		initLL(AtomSet.listItems (!tokens));
		initialiseLLTable xs
	)
	end				

  | initialiseLLTable [] = ()

val _ = initialiseLLTable (AtomSet.listItems (!symbols)) 

val prodList = AtomMap.listItemsi (!rules)

fun printRHSTable (x :: xs) = 
		let
			val _ = printAtomList x 
			val _ = print(" ,")
		in
			printRHSTable xs
		end
  | printRHSTable [] = ()

fun createLLTable (x :: xs) = 
	let 
		val (lhs, rhs) = x
		val rhsList = RHSSet.listItems rhs
		fun iterateList (lhs, x :: xs) = 
			let
				val lhsFollowSet = valOf(AtomMap.find(!followMap, lhs));
				val lhsFollowList = AtomSet.listItems lhsFollowSet;
				fun updateTable(symbol, token, rhs) = 
					let
						val rhsSet = RHSSet.singleton rhs
						val oldSet = valOf(LLMap.find(!llTable, (symbol,token)))
						val newSet = RHSSet.add(oldSet, rhs)
						val myflag = RHSSet.isSubset(rhsSet, oldSet)
						
					in
						if (RHSSet.member(oldSet, rhs) = true) then
						(
							
						)
						else
						(
							llTable := LLMap.insert(!llTable, (symbol, token), newSet)
						)

					end

				fun updateMyList ((lhs,rhs),(x::xs)) =
					(
						updateTable(lhs, x, rhs);
						updateMyList((lhs,rhs), xs)
					) 
				  | updateMyList ((lhs,rhs),[]) = ()


				fun iterateRHS ((lhs,rhs), (a :: b)) = 
					let
						fun newUpdate () =
							let 	
								val aSet = valOf(AtomMap.find(!firstMap, a));
								val aList = AtomSet.listItems aSet;
								val myNullFlag = AtomSet.member(!nullable, a);
							in
								if (myNullFlag = true) then
								(
									updateMyList ((lhs,rhs), aList);
									iterateRHS ((lhs,rhs), b)
								)
								else
									updateMyList ((lhs,rhs), aList)
							end


					in
						if (AtomSet.member(!tokens, a) = true) then
							newUpdate ()
						else if (AtomSet.member(!symbols,a) = true) then
							newUpdate ()
						else if ((Atom.toString a) = "ep") then
							updateMyList ((lhs,rhs), lhsFollowList)
						else
							()
				    end

				  | iterateRHS ((lhs,rhs), []) = updateMyList((lhs,rhs), lhsFollowList) 
			in
			(
				
					iterateRHS ((lhs,x), x);
					iterateList (lhs, xs)
				
			)
			end
		  | iterateList (lhs, []) = () 
	in
		iterateList(lhs, rhsList);
		createLLTable xs
	end
  
  | createLLTable (nil) = ()




val _ = createLLTable prodList; 



fun printLLTable () = let 
							val prodList = LLMap.listItemsi (!llTable) 
							fun listTraversal (x :: xs) = 
								let 
									val ((symbol,token), rhs) = x 								
									fun printProd ((a :: b), counter) = 
											let 
												val _ = print ("(")
												val _ = printAtom symbol
												val _ = print (", ")
												val _ = printAtom token
												val _ = print (") ")
												val _ = print ("=> ")
												val _ = printAtom symbol
												val _ = print(" --> ")
												val _ = printAtomList a
												(*val _ = print (Int.toString counter)*)
												val _ = print ("\n")
												val counter = counter + 1
											in
												printProd (b, counter)
											end
									  | printProd ([], counter) = ()


									in
										(
											printProd (RHSSet.listItems rhs, 1);
											listTraversal (xs)
										)												
								end
							  | listTraversal (nil) = ()


						 in
							listTraversal prodList

					     end

