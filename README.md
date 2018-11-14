# pilib-hs-c20182
Implementacao Pi lib em haskell, como projeto para o curso de compiladores na UFF em 2018.2

Para compilar o projeto:

va na pasta raiz do projeto, abra a um terminal e digite:
`cabal configure --enable-tests`
`cabal build`
ou simplesmente
`make`
o executavel do repl para a linguagem imp estara disponivel na pasta dist\build\impterpreter\
o executavel dos testes rodados estara disponivel na pasta dist\build\p#tests\

# TODO
- implementar bindable values: integer, boolean ou location
- adicionar Dec aos statements
- adicionar ValRef(Id) as expressoes
- adicionar tipo Dec como sendo Bind(Id, Exp), DSeq(Dec, Dec)
- adicionar Blk(Dec, Cmd) aos Cmds
