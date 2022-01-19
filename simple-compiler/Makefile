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
	rm -f Grammar.hs

cleanTokens:
	rm -f Tokens.hs
