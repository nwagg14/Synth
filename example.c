#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "synth.h"

int main(void);
void playArp(void);

void playArp(void)
{
    int j;
    for(j = 0; j < 2; j++) {
        int i;
        for(i = 0; i < 4; i++)
        {
            playOsc(0, 250, 261.63);    
            playOsc(0, 250, 329.63);    
            playOsc(0, 250, 392);    
            restOsc(0, 1000);    
        }
    
        for(i = 0; i < 4; i++)
        {
            playOsc(0, 250, 293.66);    
            playOsc(0, 250, 349.23);    
            playOsc(0, 250, 440);    
        }
    }
}

int main(void)
{
    initSynth();
    playArp();
    sleep(10);    
    termSynth();
    return 0;
}
