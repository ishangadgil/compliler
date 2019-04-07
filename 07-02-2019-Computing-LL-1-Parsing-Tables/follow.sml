val followMap:toSet ref = ref AtomMap.empty;
val flagFollow = ref true;

fun initFollowSymbol x = 
	(followMap := AtomMap.insert(!followMap,x,AtomSet.empty))

fun initFollowSymbolList (x :: xs) = (
			initFollowSymbol x;
			initFollowSymbolList xs
		)
		
  | initFollowSymbolList (nil) = ()

fun initFollowToken x = 
	(followMap := AtomMap.insert(!followMap,x,AtomSet.singleton x))

fun initFollowTokenList (x :: xs) = (
			initFollowToken x;
			initFollowTokenList xs
		)
		
  | initFollowTokenList (nil) = ()


(*initFollowialise First Map*)

val _ = initFollowSymbolList (AtomSet.listItems (!symbols))
val _ = initFollowTokenList (AtomSet.listItems (!tokens))

fun updateFollowSet (x, y) = 
	let
		val xSet = valOf(AtomMap.find(!followMap, x));
		val ySet = valOf(AtomMap.find(!firstMap, y));
		val newSet = AtomSet.union(xSet, ySet);
		val followSetFlag = AtomSet.compare(xSet,newSet);
	in 
		if(followSetFlag = EQUAL) then
  		()
		else
	  	(
	  		followMap := AtomMap.insert(!followMap, x, newSet);
	  		flagFollow := true
	  	)

	end

fun updateFollowNullSet (x, y) = 
	let
		val xSet = valOf(AtomMap.find(!followMap, x));
		val ySet = valOf(AtomMap.find(!followMap, y));
		val newSet = AtomSet.union(xSet, ySet);
		val followSetFlag = AtomSet.compare(xSet,newSet);
	in 
		if(followSetFlag = EQUAL) then
  		()
		else
	  	(
	  		followMap := AtomMap.insert(!followMap, x, newSet);
	  		flagFollow := true
	  	)

	end

fun iterateFollowRHS (lhs, (a :: b)) = 
	let
		fun newFollowUpdate (x :: xs) =
			let 	
				fun iterateEach (y :: z) = 
					if (AtomSet.member(!nullable, y) = true) then
					(
						updateFollowSet (x , y);
						iterateEach (z)
					)
					else
					(
						updateFollowSet (x, y)
					)

				  | iterateEach [] = 
				  		(updateFollowNullSet (x,lhs))
					
					
			in
			(
				iterateEach xs;
				newFollowUpdate xs
			)
			end

		  | newFollowUpdate [] = 
		  	( ) 
	in
		if (AtomSet.member(!tokens, a) = true) then
			newFollowUpdate (a :: b)
		else if (AtomSet.member(!symbols,a) = true) then
			newFollowUpdate (a :: b)
		else
			()
    end

  | iterateFollowRHS (lhs, []) = () 

fun iterateFollowList (lhs, x :: xs) = 
	(
		iterateFollowRHS (lhs, x);
		iterateFollowList (lhs, xs)
	)
  | iterateFollowList (nill) = ()

fun iterateFollow (x :: xs) = 
	let
		val (lhs,rhs) = x
		val rhsList = RHSSet.listItems rhs
	in
		iterateFollowList (lhs,rhsList);
		iterateFollow xs
	end
  

  | iterateFollow (nill) = ()


fun createFollow () = 
	if (!flagFollow = true) then
		(
			flagFollow := false;
			iterateFollow prodList;
			createFollow ()
		)
	else
		()

val _ = createFollow ();

fun printFollow () = let 
								val prodList = AtomMap.listItemsi (!followMap) 
								fun printFollowTraversal (x :: xs) = 
									let 
										val (lhs, rhs) = x 
										fun printFol (a) = 
																let 
																	fun pfollow () = 
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
																		pfollow ()
																	else
																		()
																end
										  


										in
											(
												printFol (AtomSet.listItems rhs);
												printFollowTraversal (xs)
											)												
									end
								  | printFollowTraversal (nil) = ()


							 in
								printFollowTraversal prodList

						     end