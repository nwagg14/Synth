# Synth
Sound synthesizer using C &amp; PortAudio, with a Haskell front-end

#Compiling
A makefile has been setup to build Synth. Typing 'make' in the terminal will produce the following executables 
as long as your dependencies are set-up correctly:
* example : A C program that plays some tones in a loop.
* example-hs : A similar executable to example, but written in Haskell
* mary-hs : Haskell program that plays 'Mary Had A Little Lamb'

#Dependencies
Synth depends on stdlib's math library, pthreads, and PortAudio V19

#Using Synth

Synth is built around a simple interface which allows users to 'schedule' notes on different instruments or oscillators.

The interface is defined by the following C functions:
* initSynth() : Initializes Synth for use
* termSynth() : Waits for every oscillator to finish playing it's scheduled notes, then shuts down Synth
* playOsc(int id, int ms, double hz) : plays a note with the frequency of hz for ms milliseconds on the oscillator with the given id.
* restOsc(int id, int ms) : Keeps the oscillator with the given id from playing a sound for ms milliseconds
* endOsc(int id) : Signals the oscillator that it's done playing. termSynth won't finish until this is called for all oscillators
