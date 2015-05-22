#include <stdio.h>
#include <stdlib.h>
#include "sound.h"

int main(void);
void playArp(void);

void playArp(void)
{
    int j;
    for(j = 0; j < 2; j++) {
        int i;
        for(i = 0; i < 4; i++)
        {
            playSin(250, 261.63);    
            playSin(250, 329.63);    
            playSin(250, 392);    
        }
    
        for(i = 0; i < 4; i++)
        {
            playSin(250, 293.66);    
            playSin(250, 349.23);    
            playSin(250, 440);    
        }
    }
}

int main(void)
{
    initPlayer();
    playArp();
    cleanPlayer();
    return 0;
}
