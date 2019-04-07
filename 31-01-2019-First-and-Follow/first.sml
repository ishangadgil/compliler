type toSet = AtomSet.set AtomMap.map
val firstMap:toSet ref = ref AtomMap.empty;
val flagFirst = ref true;

fun initSymbol x = 
	(firstMap := AtomMap.insert(!firstMap,x,AtomSet.empty))

fun initSymbolList (x :: xs) = (
			initSymbol x;
			initSymbolList xs
		)
		
  | initSymbolList (nil) = ()

fun initToken x = 
	(firstMap := AtomMap.insert(!firstMap,x,AtomSet.singleton x))

fun initTokenList (x :: xs) = (
			initToken x;
			initTokenList xs
		)
		
  | initTokenList (nil) = ()


(*Initialise First Map*)

val _ = initSymbolList (AtomSet.listItems (!symbols))
val _ = initTokenList (AtomSet.listItems (!tokens))

val prodList = AtomMap.listItemsi (!rules) 

fun updateSet (lhs, newSet, mySetFlag) = 
	if(mySetFlag = EQUAL) then
  		()
  	else
  	(
  		firstMap := AtomMap.insert(!firstMap, lhs, newSet);
  		flagFirst := true
  	)

fun iterateRHS (lhs, (a :: b)) = 
	let
		fun newUpdate () =
			let 	
				val oldSet = valOf(AtomMap.find(!firstMap, lhs));
				val aSet = valOf(AtomMap.find(!firstMap, a));
				val newSet = AtomSet.union(oldSet, aSet);
				val myNullFlag = AtomSet.member(!nullable, a);
				val mySetFlag = AtomSet.compare(oldSet,newSet);
			in
				if (myNullFlag = true) then
				(
					updateSet (lhs, newSet, mySetFlag);
					iterateRHS (lhs, b)
				)
				else
					updateSet (lhs, newSet, mySetFlag)
			end
	in
		if (AtomSet.member(!tokens, a) = true) then
			newUpdate ()
		else if (AtomSet.member(!symbols,a) = true) then
			newUpdate ()
		else
			()
    end

  | iterateRHS (lhs, []) = () 

fun iterateList (lhs, x :: xs) = 
	(
		iterateRHS (lhs, x);
		iterateList (lhs, xs)
	)
  | iterateList (nill) = ()


fun iterateFirst (x :: xs) = 
	let
		val (lhs,rhs) = x
		val rhsList = RHSSet.listItems rhs
	in
		iterateList (lhs,rhsList);
		iterateFirst xs
	end
  

  | iterateFirst (nill) = ()


fun createFirst () = 
	if (!flagFirst = true) then
		(
			flagFirst := false;
			iterateFirst prodList;
			createFirst ()
		)
	else
		()

val _ = createFirst ();

fun printFirst () = let 
								val prodList = AtomMap.listItemsi (!firstMap) 
								fun printFirstTraversal (x :: xs) = 
									let 
										val (lhs, rhs) = x 
										fun printF (a) = 
																let 
																	fun pfirst () = 
																		let
																			val _ = printAtom lhs
																			val _ = print ("-->")
																			val _ = printAtomList a
																			val _ = print ("\n")
																		in 
																			()
																		end
																in
																	if(AtomSet.member(!symbols,lhs) = true) then
																		pfirst ()
																	else 
																		()

																end
										  


										in
											(
												printF (AtomSet.listItems rhs);
												printFirstTraversal (xs)
											)												
									end
								  | printFirstTraversal (nil) = ()


							 in
								printFirstTraversal prodList

						     end
