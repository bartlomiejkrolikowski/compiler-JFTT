all: Tokens.hs Grammar.hs

Tokens.hs:
	alex Tokens.x

Grammar.hs:
	happy Grammar.y

clean:
	rm -f *.o *.hi
