structure Pretty : sig val printer: Ast.exp -> unit end = 

struct

       	val count : int ref = ref 0
        structure A = Ast

        val file = TextIO.openOut "output"

        fun opSymbol A.plusOp = " + "
          | opSymbol A.minOp = " - "
          | opSymbol A.mulOp = " * "
          | opSymbol A.divOp = " / "
          | opSymbol A.eqOp   = " = " 

        fun tab (0) = (TextIO.output(file, ""))
        	| tab (x) = ((TextIO.output(file, "\t"); tab(x - 1)) )
        fun printer (A.letExp{decs, body, pos}) = ( TextIO.output(file, "let\n\t"); 
                                               printer decs; 
                                               TextIO.output(file,"\nin\n\t"); 
                                               printer body; 
                                               TextIO.output(file,"\nend\n")) 

         (*| printer  (A.expList (x::(A.expList xs))) = (TextIO.output(file, "\n\t"); printer x; printer xs)
         | printer  (A.expList ([]))    = ()*)

         | printer  (A.opExp{left, oper, right, pos}) = (printer left; 
                                                    TextIO.output(file, opSymbol oper); 
                                                    printer right)

         | printer  (A.ifExp{cond, body1, body2, pos}) = (
                                                    
                                                     
                                                     TextIO.output(file,"if "); 
         											                       count := !count + 1;
                                                     printer cond; 
                                                     TextIO.output(file," then\n");
                                                     
                                                     tab (!count);
                                                     printer body1;
                                                     TextIO.output(file,"\n");
                                                     
                                                     tab (!count - 1);
                                                     TextIO.output(file,"else\n");
                                                     
                                                     tab (!count);
                                                     printer body2;
                                                     count := !count - 1;

                                                     TextIO.output(file, "\n"))

         | printer  (A.stringExp(s, p)) = TextIO.output(file, s)

         | printer  (A.intExp(i)) = TextIO.output(file, Int.toString(i))
end
