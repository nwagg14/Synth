all: example example-hs mary-hs

mary-hs: synth.o Synth.hs Mary.hs
	ghc --make -lm -lportaudio -main-is Mary -o mary-hs Mary.hs synth.o

example-hs: synth.o Synth.hs Example.hs
	ghc --make -lm -lportaudio -main-is Example -o example-hs Example.hs synth.o

example: synth.o example.o
	gcc -Wall -lm -lportaudio synth.o example.o -o example
example.o: example.c
	gcc -Wall -c example.c -o example.o

synth.o: synth.c
	gcc -Wall -c synth.c -o synth.o

clean:
	rm *.o *.hi example example-hs mary-hs
