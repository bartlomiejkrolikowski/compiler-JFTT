all: all_hs
	ghc --make kompilator.hs

all_hs: Tokens.hs Grammar/Parse.hs

Tokens.hs:
	alex Tokens.x

Grammar/Parse.hs:
	happy Grammar/Parse.y

test: all_hs
	ghc --make testCode.hs

clean:
	rm -f *.o *.hi */*.o */*.hi testCode kompilator

cleanGrammar:
	rm -f Grammar/Parse.hs

cleanTokens:
	rm -f Tokens.hs
