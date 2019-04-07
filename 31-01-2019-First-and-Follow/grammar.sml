type RHS = Atom.atom list  (* The RHS γ of a rule A -> γ *)

(*

We have the structures AtomSet and AtomMap to represent sets and maps
of Atoms. For any type t if we want sets and maps (dictionaries) we
need an ordering structure on the elements.  We would like to create
the set structure on RHS's. For this you first need to define a
structure of signature ORD_KEY for RHS.

*)

structure RHS_KEY : ORD_KEY = struct
	type ord_key = RHS
	fun compare ((x::xs), (y::ys)) = if Atom.lexCompare (x,y) = EQUAL
									 	then compare(xs,ys)
									 else
										 Atom.lexCompare(x,y) 
	| compare(_,x::xs) = LESS 
	| compare(_,_) = GREATER

end


(*

Use the above structure to create a set of rhs's

*)

structure RHSSet = RedBlackSetFn (RHS_KEY)

type Productions = RHSSet.set

(* The rules of the grammar are a dictionary whose keys are the symbol
   and the values are the Productions associated with the grammar.
*)

type Rules = Productions AtomMap.map


type Grammar    = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }


fun printAtom a = (print (Atom.toString a); print(" "))
fun printAtomList (x :: xs) = (printAtom x; printAtomList xs)
	| printAtomList [] = print("")

fun createAtomList (x :: xs) = [Atom.atom x]@createAtomList xs
  | createAtomList [] = [] 

fun createStringList (x :: xs) = [Atom.toString x]@createStringList xs
  | createStringList [] = []

val symbols = ref AtomSet.empty;
val tokens = ref AtomSet.empty;
val rules:Rules ref = ref AtomMap.empty;



fun addSymbol sym = let val symAtom = Atom.atom sym in
						(symbols := AtomSet.add(!symbols, symAtom))
					end

fun addSymbolList symList = let val symAtomList = createAtomList symList in
								(symbols := AtomSet.addList (!symbols, symAtomList))
							end

fun addToken tok = let val tokAtom = Atom.atom tok in
						(tokens := AtomSet.add(!tokens, tokAtom))
					end

fun addTokenList tokList = let val tokAtomList = createAtomList tokList in
								(tokens := AtomSet.addList (!tokens, tokAtomList))
							end


fun createAtomL2 (x :: xs) = [createAtomList(x)]@createAtomL2 xs
  | createAtomL2 [] = []


fun addRule (lhs, rhs) = let val rhsAtom = createAtomL2(rhs) in
							(rules := AtomMap.insert(!rules, Atom.atom lhs, RHSSet.fromList rhsAtom))
						 end

fun printProductions () = let 
								val prodList = AtomMap.listItemsi (!rules) 
								fun listTraversal (x :: xs) = 
																let 
																	val (lhs, rhs) = x 
																	fun printProd (a :: b) = 
																							let 
																								val _ = printAtom lhs
																								val _ = print ("-->")
																								val _ = printAtomList a
																								val _ = print ("\n")
																							in
																								printProd b
																							end
																	  | printProd (nil) = ()


																	in
																		(
																			printProd (RHSSet.listItems rhs);
																			listTraversal (xs)
																		)												
																end
								  | listTraversal (nil) = ()


							 in
								listTraversal prodList

						     end

fun printSingleProd (lhs,rhs) = let 
									fun printProd (a :: b) = 
										let 
											val _ = printAtom lhs
											val _ = print ("-->")
											val _ = printAtomList a
											val _ = print ("\n")
											in
												printProd b
											end
									  | printProd (nil) = ()

								in
									(
										printProd (RHSSet.listItems rhs)
									)											
								end