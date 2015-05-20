#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "portaudio.h"

#define SAMPLE_RATE (44100)
#define FRAMES_PER_BUFFER (1050)

#define TABLE_SIZE (210)

void error(PaError err)
{
    exit(-1);
}

float sine[TABLE_SIZE];

void playSin(PaStream *stream, int ms, double hz) 
{
    int i, j;
    static double left_phase = 0;
    static double right_phase = 0;
    
    float buffer[FRAMES_PER_BUFFER][2];
    int bufferCount = (ms * SAMPLE_RATE) / (FRAMES_PER_BUFFER * 1000); 
    PaError  err;

    printf("hz: %f\n", hz);
    for(i = 0; i < bufferCount; i++)
    {
        for(j = 0; j < FRAMES_PER_BUFFER; j++)
        {
            buffer[j][0] = sine[(int)left_phase]; 
            buffer[j][1] = sine[(int)right_phase];
            left_phase = (left_phase + hz/TABLE_SIZE);
            right_phase = (right_phase + hz/TABLE_SIZE);
            if(left_phase >= TABLE_SIZE) left_phase = left_phase - TABLE_SIZE;
            if(right_phase >= TABLE_SIZE) right_phase = right_phase - TABLE_SIZE;
        }
        err = Pa_WriteStream(stream, buffer, FRAMES_PER_BUFFER);
        if (err != paNoError) 
        {
            printf("WriteStream failed\n");
            error(err);
        }
    }
}

void playArp(PaStream *stream)
{

    PaError  err;
    err = Pa_StartStream(stream);
    if (err != paNoError) 
    {
        printf("StartStream failed\n");
        error(err);
    }
        
    int j;
    for(j = 0; j < 2; j++) {
    int i;
    for(i = 0; i < 4; i++)
    {
        playSin(stream, 350, 261.63);    
        playSin(stream, 350, 329.63);    
        playSin(stream, 350, 392);    
    }
    
    for(i = 0; i < 4; i++)
    {
        playSin(stream, 350, 293.66);    
        playSin(stream, 350, 349.23);    
        playSin(stream, 350, 440);    
    }
    }
    
    for(j = 0; j < 4; j++)
    {
        playSin(stream, 350, 440);    
        playSin(stream, 350, 349.23);    
        playSin(stream, 350, 293.66);    
    }
    
    err = Pa_StopStream(stream);
    if (err != paNoError) 
    {
        printf("StopStream failed\n");
        error(err);
    }
}

int main(void)
{
    PaStreamParameters outputParameters;
    PaStream *stream;
    PaError  err;
    int left_inc = 1;
    int right_inc = 3;
    int i, j, k;
    
    for(i = 0; i < TABLE_SIZE; i++)
    {
        sine[i] = (float) sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. );
    }

    err = Pa_Initialize();
    if (err != paNoError) 
    {
        printf("Failed to initialize\n");
        error(err);
    }

    outputParameters.device = Pa_GetDefaultOutputDevice();
    if (outputParameters.device == paNoDevice) {
        printf("No default output device\n");
        error(paNoError);
    }
    outputParameters.channelCount = 2;
    outputParameters.sampleFormat = paFloat32;
    outputParameters.suggestedLatency = 0.050;
    outputParameters.hostApiSpecificStreamInfo = NULL;
    
    err = Pa_OpenStream( &stream, NULL, &outputParameters, SAMPLE_RATE, 
                        FRAMES_PER_BUFFER, paNoFlag, NULL, NULL);

    if (err != paNoError) 
    {
        printf("Failed to open stream\n");
        error(err);
    }

    playArp(stream);

    err = Pa_CloseStream(stream); 
    if (err != paNoError) 
    {
        printf("Failed to close stream\n");
        error(err);
    }

    Pa_Terminate();
    return 0;
}
