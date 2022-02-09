all: all_hs
	ghc --make kompilator.hs

all_hs: Tokens.hs Grammar/Parse.hs

Tokens.hs:
	alex Tokens.x

Grammar/Parse.hs:
	happy Grammar/Parse.y

test: all_hs testCode testGrammar testInter

testCode:
	ghc --make testCode.hs

testGrammar:
	ghc --make testGrammar.hs

testInter:
	ghc --make testInter.hs

clean:
	rm -f *.o *.hi */*.o */*.hi testCode testGrammar testInter kompilator

cleanGrammar:
	rm -f Grammar/Parse.hs

cleanTokens:
	rm -f Tokens.hs
