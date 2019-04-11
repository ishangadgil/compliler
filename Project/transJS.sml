structure Translate :
  sig val transPrint  : TextIO.outstream * Absyn.exp -> unit end  = 

struct

       	val count : int ref = ref 0
        structure A = Absyn
        structure S = Symbol

        fun transPrint (outstream, exp) = 
        let
        fun say s =  TextIO.output(outstream,s)

        fun opSymbol A.PlusOp = " + "
          | opSymbol A.MinusOp = " - "
          | opSymbol A.TimesOp = " * "
          | opSymbol A.DivideOp = " / "
          | opSymbol A.EqOp   = "== " 
          | opSymbol A.NeqOp = "!="
          | opSymbol A.LtOp = "<"
          | opSymbol A.LeOp = "<="
          | opSymbol A.GtOp = ">"
          | opSymbol A.GeOp = ">="

        fun tab (0) = (say ( ""))
        	| tab (x) = ((say ( "\t"); tab(x - 1)) )

      fun printTyFieldsAux ({name= ID1, escape=(ref true), typ= ID2, pos=ID1left}:A.field) = S.printSymbol (outstream,ID1)
        | printTyFieldsAux _ = ()

      fun printTyFieldsA (x :: xs) = (say (", ");printTyFieldsAux x; printTyFieldsA xs)
        | printTyFieldsA ([]) = ()

      fun printTyFields (x :: xs) = (printTyFieldsAux x; printTyFieldsA xs)
        | printTyFields ([]) = ()

      fun printVarDec ({name= ID, escape=ref true, typ=NONE, init=exp, pos=VARleft}) = 
                        (say ("var ");
                         S.printSymbol (outstream,  ID);
                         say (" = ");
                         trans exp
                        )
        | printVarDec ({name=ID1, escape=ref true, typ=SOME(ID2,ID2left), init=exp, pos=VARleft}) = 
                      (say ("var ");
                       S.printSymbol (outstream,  ID1);
                       say (" = ");
                       trans exp
                      )
       

      and printFunDecAux ({name=ID,params=tyfields, result=NONE, body=exp, pos=FUNCTIONleft}:A.fundec) = 
                            (say ("function ");
                             S.printSymbol (outstream,ID);
                             say ("(");
                             printTyFields tyfields;
                             say (") {\n");
                             trans exp;
                             say ("\n}\n")
                            ) 
      and printFunDec (x::xs) = 
                         (printFunDecAux x;
                          printFunDec xs
                         )
        | printFunDec ([]) = ()

      and printDecsAux (A.TypeDec(tydecs)) = ()
        | printDecsAux (A.VarDec(vardec)) = printVarDec vardec
        | printDecsAux (A.FunctionDec(fundecs)) = printFunDec fundecs 
      and  printDecs (x::xs) =
              (printDecsAux x;
               say (";\n");
               printDecs xs
              )
        | printDecs ([]) = ()        

      and transVar (A.SimpleVar(symbol,pos)) = S.printSymbol (outstream,symbol)
          | transVar (A.FieldVar(var,symbol,pos)) = 
                     (transVar var;
                      S.printSymbol (outstream,symbol)                   
                     )
          | transVar (A.SubscriptVar(var,exp,pos)) = 
                     (transVar var;
                      trans exp
                     )

     and    transSeq (x,pos) = trans x
     and    printArgsAux(x :: xs) =  (say(", "); trans x; printArgsAux xs)
          | printArgsAux ([]) = ()
     and    printArgs (x :: xs) = (trans x; printArgsAux xs)
          | printArgs ([]) = ()
     and    transSeqAux (x :: xs) =
                        ( 
                          transSeq x;
                          say  ";\n";
                          transSeqAux xs
                        )
          |  transSeqAux ([]) = ()

     and    trans (A.IntExp(x)) = say (Int.toString(x))
          | trans (A.StringExp(s, pos)) = say ( s)
          | trans (A.OpExp{left, oper, right, pos}) = 
                  (trans left; 
                   say ( opSymbol oper); 
                   trans right
                   )
          | trans (A.SeqExp(x)) = 
                  (
                    say ("(\n");
                    transSeqAux x;
                    say (")\n")
                  )

          | trans (A.BreakExp(BREAKleft)) = say ("break;")

          | trans (A.VarExp(var)) = transVar var
          

          | trans (A.IfExp({test=exp1, then'=exp2, else'=NONE, pos=IFleft})) = 
                  (say ( "if (");
                   trans exp1;
                   say ( ") {\n");
                   trans exp2;
                   say ( "\n}\n")
                   )
          | trans (A.IfExp({test=exp1, then'=exp2, else'=SOME exp3, pos=IFleft})) = 
                  (say ( "if (");
                   trans exp1;
                   say ( ") {\n");
                   trans exp2;
                   say ( "\n} else {\n");
                   trans exp3;
                   say ( "\n}\n")
                   )
          | trans (A.WhileExp({test=exp1, body=exp2, pos=WHILEleft})) = 
                  (say ( "while (");
                   trans exp1;
                   say ( ") {\n");
                   trans exp2;
                   say ( "\n}\n")
                  ) 
          | trans (A.ForExp({var= ID, escape=(ref true), lo=exp1, hi=exp2, body=exp3, pos=FORleft})) = 
                  (say ("for (");
                   S.printSymbol (outstream,ID);
                   say (" = ");
                   trans exp1;
                   say ("; ");
                   S.printSymbol (outstream,ID);
                   say (" <= ");
                   trans exp2;
                   say ("; ");
                   S.printSymbol (outstream,ID);
                   say ("++) {\n");
                   trans exp3;
                   say ("\n}\n")
                  )
          | trans (A.CallExp({func= ID, args=funarg, pos=IDleft})) = 
                  (
                    say ("function ");
                    S.printSymbol (outstream,ID);
                    printArgs funarg;
                    say (");\n")
                  )
          | trans (A.ArrayExp({typ=ID, size=exp1, init=exp2, pos=IDleft})) = 
                (say ("var ");
                 S.printSymbol (outstream, ID);
                 say("[");
                 trans exp1;
                 say("] = ");
                 trans exp2;
                 say (";\n")
                )

          | trans (A.NilExp) = say ("NULL;\n")
          | trans (A.LetExp({decs=decs, body=letbody, pos=LETleft})) = 
                  (
                      say ("function letHandler"^Int.toString(!count)^"() {\n");
                      count := !count + 1;
                      printDecs decs;
                      trans letbody;
                      say ("\n}\nletHandler"^Int.toString(!count-1)^"()\n")
                  )
          | trans _ = ()  

          in
          trans(exp); say "\n"; TextIO.flushOut outstream
          end      
end
