addTokenList ["a","c","d"];
addSymbolList ["X", "Y","Z"];


addRule("X", [["a"],["Y"]]);
addRule("Y", [["ep"],["c"]]);
addRule("Z", [["X","Y","Z"],["d"]]);