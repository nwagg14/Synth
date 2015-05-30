#ifndef _SYNTH_
#define _SYNTH_

void initSynth(void);
void termSynth(void);
void playOsc(int id, int ms, double hz);
void restOsc(int id, int ms);
void endOsc(int id);

#endif
