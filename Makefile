GHC = ghc
OPTS =

SRC=Lab2.hs

all: $(SRC)
	$(GHC) $(OPTS) $^ -o $^.o

clean:
	rm -f *.o *.hi
