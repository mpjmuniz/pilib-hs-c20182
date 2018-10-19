
pilibmake:
	@brew install ghc cabal-install
	@alex Lexer.x
	@happy Parser.y
	@ghc main --make Calc.hs
