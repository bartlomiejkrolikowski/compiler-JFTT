all: all_hs
	ghc --make kompilator.hs

all_hs: Tokens.hs Grammar.hs

Tokens.hs:
	alex Tokens.x

Grammar.hs:
	happy Grammar.y

test: all_hs
	ghc --make testCode.hs

clean:
	rm -f *.o *.hi testCode kompilator

cleanGrammar:
	rm -f Grammar.hs

cleanTokens:
	rm -f Tokens.hs
