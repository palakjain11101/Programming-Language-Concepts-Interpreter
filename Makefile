DEPEND += Tokens.hs Grammar.hs Eval.hs

all: $(DEPEND) myinterpreter

myinterpreter: $(DEPEND) Main.hs
	ghc -o myinterpreter Main.hs

Grammar.hs : Grammar.y
	@rm -f Grammar.hs
	happy Grammar.y
	@chmod -w Grammar.hs

Tokens.hs : Tokens.x
	@rm -f Tokens.hs
	alex Tokens.x
	@chmod -w Tokens.hs

clean::
	rm -rf Tokens.hs Grammar.hs *.hi *.o *.info myinterpreter

