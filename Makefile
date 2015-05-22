all: example example-hs mary-hs

mary-hs: sound.o Sound.hs Mary.hs
	ghc --make -lm -lportaudio -main-is Mary -o mary-hs Mary.hs sound.o

example-hs: sound.o Sound.hs Example.hs
	ghc --make -lm -lportaudio -main-is Example -o example-hs Example.hs sound.o

example: sound.o example.o
	gcc -Wall -lm -lportaudio sound.o example.o -o example
example.o: example.c
	gcc -Wall -c example.c -o example.o

sound.o: sound.c
	gcc -Wall -c sound.c -o sound.o

clean:
	rm *.o *.hi example example-hs mary-hs
