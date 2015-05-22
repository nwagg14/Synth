example-hs: sound.o Sound.hs
	ghc --make -lm -lportaudio -main-is Sound -o example-hs Sound.hs sound.o

example: sound.o example.o
	gcc -Wall -lm -lportaudio sound.o example.o -o example
example.o: example.c
	gcc -Wall -c example.c -o example.o

sound.o: sound.c
	gcc -Wall -c sound.c -o sound.o

clean:
	rm *.o example example-hs
