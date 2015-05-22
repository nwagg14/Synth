#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "portaudio.h"
#include "synth.h"

#define SAMPLE_RATE (44100)
#define FRAMES_PER_BUFFER (210)

#define TABLE_SIZE (210)

void error(PaError err);

void error(PaError err)
{
    printf("Error message: %s\n", Pa_GetErrorText(err));
    exit(-1);
}

/* Globals */
float sine[TABLE_SIZE];
PaStream *stream;

void playSin(int ms, double hz) 
{
    int i, j;

    static double left_phase = 0;
    static double right_phase = 0;
    float vol = 0;   
    float buffer[FRAMES_PER_BUFFER][2];
    int bufferCount = (ms * SAMPLE_RATE) / (FRAMES_PER_BUFFER * 1000); 
    PaError  err;

    for(i = 0; i < bufferCount; i++)
    {
        for(j = 0; j < FRAMES_PER_BUFFER; j++)
        {
            // ramp up volume for first frame
            if(i == 0)
            {
                vol = (j + 1) / (float)(FRAMES_PER_BUFFER);
            }

            // ramp down volume for last frame
            else if (i == bufferCount -1)
            {
                vol = 1.0 - ((j + 1) / (float)(FRAMES_PER_BUFFER));
            }
            else vol = 1;

            // grab values from lookup table
            buffer[j][0] = vol * sine[(int)left_phase]; 
            buffer[j][1] = vol * sine[(int)right_phase];

            // update lookup table indices
            left_phase = (left_phase + hz/TABLE_SIZE);
            right_phase = (right_phase + hz/TABLE_SIZE);
            if(left_phase >= TABLE_SIZE) left_phase = left_phase - TABLE_SIZE;
            if(right_phase >= TABLE_SIZE) right_phase = right_phase - TABLE_SIZE;
        }

        // write a buffer frame to the stream
        err = Pa_WriteStream(stream, buffer, FRAMES_PER_BUFFER);
        if (err != paNoError) 
        {
            printf("WriteStream failed\n");
            error(err);
        }
    }
}

void initSynth(void)
{
    PaStreamParameters outputParameters;
    PaError  err;

    /* Initialize the sine wave lookup table */
    int i;
    for(i = 0; i < TABLE_SIZE; i++)
    {
        sine[i] = (float) sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. );
    }

    /* initialize PortAudio, and exit if theres an error */
    err = Pa_Initialize();
    if (err != paNoError) 
    {
        printf("Failed to initialize\n");
        error(err);
    }
    

    /* Define parameters for output device */
    outputParameters.device = Pa_GetDefaultOutputDevice();
    if (outputParameters.device == paNoDevice) {
        printf("No default output device\n");
        error(paNoError);
    }
    outputParameters.channelCount = 2;
    outputParameters.sampleFormat = paFloat32;
    outputParameters.suggestedLatency = 0.050;
    outputParameters.hostApiSpecificStreamInfo = NULL;


    /* Open a output-only stream using blocking I/O */
    err = Pa_OpenStream( &stream, NULL, &outputParameters, SAMPLE_RATE, 
                        FRAMES_PER_BUFFER, paNoFlag, NULL, NULL);
    if (err != paNoError) 
    {
        printf("Failed to open stream\n");
        error(err);
    }
    
    /* Start the stream */
    err = Pa_StartStream(stream);
    if (err != paNoError) 
    {
        printf("StartStream failed\n");
        error(err);
    }
}

void termSynth(void)
{
    PaError  err;
    
    err = Pa_StopStream(stream);
    if (err != paNoError) 
    {
        printf("StopStream failed\n");
        error(err);
    }

    err = Pa_CloseStream(stream); 
    if (err != paNoError) 
    {
        printf("Failed to close stream\n");
        error(err);
    }
    Pa_Terminate();
}

