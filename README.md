# pilib-hs-c20182
Pi lib implementation on haskell, as the project for the Compilers course at UFF, 2018.2


Para rodar o projeto:

alex Lexer.x

happy Parser.y

ghc --make Calc.hs   (verificar no código o caminho para o arquivo que será lido)
