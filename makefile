
pilibmake:
	@cabal configure --enable-tests
	@cabal build
	@dist\build\impterpreter\impterpreter #executavel para interpretador on the fly
	#@dist\build\p3-tests\p3-tests #executavel para testes da p3
