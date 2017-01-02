module.exports = {
  adtCoreFn: {"M":{"imports":["Prim","M"],"builtWith":"0.10.3","exports":["A","B","a","b"],"decls":[{"A":["Constructor","T","A",[]]},{"B":["Constructor","T","B",[]]},{"b":["Var","M.B"]},{"a":["Var","M.A"]}],"foreign":[]}},
  applicationCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["apply"],"decls":[{"apply":["Abs","f",["Abs","x",["App",["Var","f"],["Var","x"]]]]}],"foreign":[]}},
  arrayCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["array"],"decls":[{"array":["Literal",["ArrayLiteral",[["Literal",["IntLiteral",0]],["Literal",["IntLiteral",1]]]]]}],"foreign":[]}},
  booleanCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["boolean"],"decls":[{"boolean":["Literal",["BooleanLiteral",false]]}],"foreign":[]}},
  caseCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["identity"],"decls":[{"identity":["Abs","x",["Case",[["Var","x"]],[[[["VarBinder","y"]],["Var","y"]]]]]}],"foreign":[]}},
  characterCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["char"],"decls":[{"char":["Literal",["CharLiteral","a"]]}],"foreign":[]}},
  conditionalCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["not"],"decls":[{"not":["Abs","x",["Case",[["Var","x"]],[[[["LiteralBinder",["BooleanLiteral",true]]],["Literal",["BooleanLiteral",false]]],[[["LiteralBinder",["BooleanLiteral",false]]],["Literal",["BooleanLiteral",true]]]]]]}],"foreign":[]}},
  emptyCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":[],"decls":[],"foreign":[]}},
  functionCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["identity"],"decls":[{"identity":["Abs","x",["Var","x"]]}],"foreign":[]}},
  identifierCoreFn: {"M":{"imports":["Prim","M"],"builtWith":"0.10.3","exports":["f","g"],"decls":[{"f":["Abs","x",["Var","x"]]},{"g":["Var","M.f"]}],"foreign":[]}},
  instanceCoreFn: {"Example":{"imports":["Prim","Example"],"builtWith":"0.10.3","exports":["C","m","cInt"],"decls":[{"C":["Abs","m",["Literal",["ObjectLiteral",{"m":["Var","m"]}]]]},{"m":["Abs","dict",["Accessor","m",["Var","dict"]]]},{"cInt":["App",["Var","Example.C"],["Literal",["IntLiteral",0]]]}],"foreign":[]}},
  integerCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["int"],"decls":[{"int":["Literal",["IntLiteral",0]]}],"foreign":[]}},
  letCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["f"],"decls":[{"f":["Abs","x",["Let",[{"y":["Var","x"]}],["Var","y"]]]}],"foreign":[]}},
  moduleNameCoreFn: {"Aa1.Bb1":{"imports":["Prim"],"builtWith":"0.10.3","exports":[],"decls":[],"foreign":[]}},
  multipleCaseCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["f"],"decls":[{"f":["Abs","x",["Case",[["Var","x"],["Var","x"]],[[[["VarBinder","y"],["VarBinder","z"]],["Var","x"]]]]]}],"foreign":[]}},
  mutualCoreFn: {"M":{"imports":["Prim","M"],"builtWith":"0.10.3","exports":["f","g"],"decls":[{"g":["Abs","x",["App",["Var","M.f"],["Var","x"]]],"f":["Abs","x",["App",["Var","M.g"],["Var","x"]]]}],"foreign":[]}},
  namedCoreFn: ﻿{"Example":{"imports":["Prim"],"builtWith":"0.10.3","exports":["named"],"decls":[{"named":["Abs","x",["Case",[["Var","x"]],[[[["NamedBinder","y","NullBinder"]],["Var","y"]]]]]}],"foreign":[]}},
  newtypeCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["X"],"decls":[{"X":["Abs","x",["Var","x"]]}],"foreign":[]}},
  nonEmptyObjectCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["x"],"decls":[{"x":["Literal",["ObjectLiteral",{"a":["Literal",["IntLiteral",1]]}]]}],"foreign":[]}},
  nullCaseCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["f"],"decls":[{"f":["Abs","x",["Case",[["Var","x"]],[[["NullBinder"],["Var","x"]]]]]}],"foreign":[]}},
  numberCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["number"],"decls":[{"number":["Literal",["NumberLiteral",0]]}],"foreign":[]}},
  objectCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["x"],"decls":[{"x":["Literal",["ObjectLiteral",{}]]}],"foreign":[]}},
  partialCoreFn: ﻿{"Example":{"imports":["Prim"],"builtWith":"0.10.3","exports":["partial"],"decls":[{"partial":["Abs","dictPartial",["Abs","v",["Let",[{"__unused":["Abs","dictPartial1",["Abs","$2",["Var","$2"]]]}],["App",["App",["Var","__unused"],["Var","dictPartial"]],["Case",[["Var","v"]],[[[["LiteralBinder",["IntLiteral",0]]],["Literal",["IntLiteral",0]]]]]]]]]}],"foreign":[]}},
  recordAccessCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["f"],"decls":[{"f":["Abs","x",["Accessor","k",["Var","x"]]]}],"foreign":[]}},
  semigroupCoreFn: {"Example":{"imports":["Prim","Example"],"builtWith":"0.10.3","exports":["Semigroup","append","triple"],"decls":[{"Semigroup":["Abs","append",["Literal",["ObjectLiteral",{"append":["Var","append"]}]]]},{"append":["Abs","dict",["Accessor","append",["Var","dict"]]]},{"triple":["Abs","dictSemigroup",["Abs","x",["App",["App",["App",["Var","Example.append"],["Var","dictSemigroup"]],["App",["App",["App",["Var","Example.append"],["Var","dictSemigroup"]],["Var","x"]],["Var","x"]]],["Var","x"]]]]}],"foreign":[]}},
  stringCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["string"],"decls":[{"string":["Literal",["StringLiteral",""]]}],"foreign":[]}},
  superClassCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["A","B"],"decls":[{"A":["Literal",["ObjectLiteral",{}]]},{"B":["Abs","__superclass_M.A_0",["Literal",["ObjectLiteral",{"__superclass_M.A_0":["Var","__superclass_M.A_0"]}]]]}],"foreign":[]}},
  typeClassCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["Semigroup","append"],"decls":[{"Semigroup":["Abs","append",["Literal",["ObjectLiteral",{"append":["Var","append"]}]]]},{"append":["Abs","dict",["Accessor","append",["Var","dict"]]]}],"foreign":[]}},
  unitCoreFn: {"M":{"imports":["Prim","M"],"builtWith":"0.10.3","exports":["UnitC","unit"],"decls":[{"UnitC":["Constructor","UnitT","UnitC",[]]},{"unit":["Var","M.UnitC"]}],"foreign":[]}},
}
