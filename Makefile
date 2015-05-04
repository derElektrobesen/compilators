BIN = Lab2 Lab3

all:
	@ghc --make Lab2
	@ghc --make Lab3

clean:
	rm -f *.o *.hi Lab2 Lab3
