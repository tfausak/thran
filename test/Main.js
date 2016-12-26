module.exports = {
  emptyCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":[],"decls":[],"foreign":[]}},
  functionCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["identity"],"decls":[{"identity":["Abs","x",["Var","x"]]}],"foreign":[]}},
  applicationCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["apply"],"decls":[{"apply":["Abs","f",["Abs","x",["App",["Var","f"],["Var","x"]]]]}],"foreign":[]}},
  booleanCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["boolean"],"decls":[{"boolean":["Literal",["BooleanLiteral",false]]}],"foreign":[]}},
  intCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["int"],"decls":[{"int":["Literal",["IntLiteral",0]]}],"foreign":[]}},
  numberCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["number"],"decls":[{"number":["Literal",["NumberLiteral",0]]}],"foreign":[]}},
  charCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["char"],"decls":[{"char":["Literal",["CharLiteral","a"]]}],"foreign":[]}},
  stringCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["string"],"decls":[{"string":["Literal",["StringLiteral",""]]}],"foreign":[]}},
  arrayCoreFn: {"M":{"imports":["Prim"],"builtWith":"0.10.3","exports":["array"],"decls":[{"array":["Literal",["ArrayLiteral",[["Literal",["IntLiteral",0]],["Literal",["IntLiteral",1]]]]]}],"foreign":[]}},
}
