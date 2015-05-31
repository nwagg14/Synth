#ifndef _SYNTH_
#define _SYNTH_

void initSynth(void);
void termSynth(void);
void playOsc(unsigned int id, unsigned int ms, double hz);
void restOsc(unsigned int id, unsigned int ms);
void endOsc(unsigned int id);

#endif
