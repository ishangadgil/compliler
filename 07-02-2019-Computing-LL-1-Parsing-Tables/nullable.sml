val nullable = ref AtomSet.empty;
val flagNullable = ref true;


fun allNullable (x :: xs) = if (AtomSet.member(!nullable, x) = true) then
								allNullable xs
							else
								false

  | allNullable (nil) = true

fun createNullable () = let 
							val prodList = AtomMap.listItemsi (!rules) 
							fun findNullable (x :: xs) = 
									let 
										val (lhs, rhs) = x 
										fun findNull (a :: b) = 
											let
												val aList = createStringList a
											in			
												if AtomSet.member(!nullable, lhs) = true then ()
												else (
													if (aList = ["ep"]) then
														(
															flagNullable := true;
															nullable := AtomSet.add(!nullable, lhs)
														)
													else if (allNullable a = true) then
														(
															flagNullable := true;
															nullable := AtomSet.add(!nullable, lhs)
														)

														else
															findNull (b)
												)
											end

																	
																
										  | findNull (nil) = ()


									in
										(
											findNull (RHSSet.listItems rhs);
											findNullable (xs)
										)												
									end
							  | findNullable (nil) = ()


						 in
							findNullable prodList

					     end

fun myNullable () = while (!flagNullable = true) do
						(
							flagNullable := false;
							createNullable ()
						)

val _ = myNullable ();

fun printNullable () = let
							val nullList = AtomSet.listItems (!nullable)
						in
							printAtomList nullList
						end