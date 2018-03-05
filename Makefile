.PHONY: repl test

repl:
	clj -Adev

test:
	clj -Atest -e deprecated
