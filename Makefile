all: example example-hs mary-hs libsynth.so

mary-hs: synth.o ringbuffer.o Synth.hs Mary.hs
	ghc --make -lm -lpthread -lportaudio -main-is Mary -o mary-hs Mary.hs synth.o ringbuffer.o

example-hs: synth.o ringbuffer.o Synth.hs Example.hs
	ghc --make -lm -lpthread -lportaudio -main-is Example -o example-hs Example.hs synth.o ringbuffer.o

example: synth.o ringbuffer.o example.o
	gcc -Wall -lm -lpthread -lportaudio synth.o ringbuffer.o example.o -o example
example.o: example.c synth.h
	gcc -Wall -c example.c -o example.o

libsynth.so: synth.o
	gcc -Wall -lm -lpthread -lportaudio -fPIC -shared synth.o ringbuffer.o -o libsynth.so

synth.o: synth.c synth.h pa_ringbuffer.h
	gcc -Wall -fPIC -g -c synth.c -o synth.o

ringbuffer.o: pa_ringbuffer.c pa_ringbuffer.h
	gcc -Wall -fPIC -c pa_ringbuffer.c -o ringbuffer.o

clean:
	rm *.o *.hi example example-hs mary-hs libsynth.so
