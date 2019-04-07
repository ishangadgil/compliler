addTokenList ["a", "b", "c","ep"];
addSymbolList ["A", "B", "C","D","E","F"];


addRule("A", [["a","B"],["a"]]);
addRule("B", [["b"],["B","C"]]);
addRule("C", [["ep"]]);
addRule("D", [["C"]]);
addRule("E", [["A","C","D"]]);
addRule("F", [["C","D"]]);
