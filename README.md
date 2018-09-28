# pilib-hs-c20182
Pi lib implementation on haskell, as the project for the Compilers course at UFF, 2018.2


Para rodar o projeto:

alex Lexer.x

happy Parser.y

--verificar no código Calc.hs, antes de compilar, o caminho para o arquivo que será lido como entrada do programa

ghc --make Calc.hs   


