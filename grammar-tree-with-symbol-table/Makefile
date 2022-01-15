all: Tokens.hs Grammar.hs

Tokens.hs:
	alex Tokens.x

Grammar.hs:
	happy Grammar.y

test: all
	ghc --make testGrammar.hs

clean:
	rm -f *.o *.hi testGrammar

cleanGrammar:
	rm Grammar.hs

cleanTokens:
	rm Tokens.hs
