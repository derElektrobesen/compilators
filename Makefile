BIN = Lab2 Lab3 Lab4

all:
	@for var in ${BIN}; do ghc --make $$var; done

clean:
	rm -f *.o *.hi ${BIN} *~
