#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "portaudio.h"
#include "pa_ringbuffer.h"
#include "synth.h"

#define SAMPLE_RATE (44100)
#define FRAMES_PER_BUFFER (210)

#define TABLE_SIZE (210)

enum note_type {
    NOTE,
    REST,
    END
};

struct note {
    enum note_type type;
    int ms;
    double hz;
};    

struct osc {

    PaUtilRingBuffer rbuf;
   
    float *table;
    double vol;
    double left_phase;
    double right_phase; 

    int frames_played;          // this will be -1 if no curr_note is available
    unsigned long int num_frames;         
    struct note curr_note;
};

void error(PaError err);

void error(PaError err)
{
    printf("Error message: %s\n", Pa_GetErrorText(err));
    exit(-1);
}

/* Globals */
float sine[TABLE_SIZE];
PaStream *stream;
struct osc sine_osc;

int paCallback(const void *inputBuffer, void *outputBuffer,
                unsigned long framesPerBuffer,
                const PaStreamCallbackTimeInfo *timeInfo,
                PaStreamCallbackFlags statusFlags,
                void *userData) {

    struct osc *osc = (struct osc*) userData;
    float *buffer = (float*)outputBuffer;

    // decide what to do if we don't have a current note
    if(osc->frames_played == -1) {
        if(PaUtil_ReadRingBuffer(&osc->rbuf, &osc->curr_note, 1) == 0) {
            // no note was read from the ring buffer 
            return paContinue;
        }

        osc->vol = 0;
        osc->frames_played = 0;
        osc->num_frames = (osc->curr_note.ms/1000.0) * SAMPLE_RATE;
    }
    
    int i; 
    for(i = 0; i < framesPerBuffer; i++)
    {
        // ramp up volume for first frame
        if(osc->frames_played == 0)
        {
            osc->vol = (i + 1) / (float)(framesPerBuffer);
        }

        // ramp down volume for last frame
        else if (osc->frames_played >= osc->num_frames - framesPerBuffer)
        {
            osc->vol = 1.0 - ((i + 1) / (float)(framesPerBuffer));
        }
        else osc->vol = 1;

        // grab values from lookup table
        *buffer++ = osc->vol * osc->table[(int)osc->left_phase]; 
        *buffer++ = osc->vol * osc->table[(int)osc->right_phase];

        // update lookup table indices
        osc->left_phase = (osc->left_phase + osc->curr_note.hz/TABLE_SIZE);
        osc->right_phase = (osc->right_phase + osc->curr_note.hz/TABLE_SIZE);
        if(osc->left_phase >= TABLE_SIZE) osc->left_phase = osc->left_phase - TABLE_SIZE;
        if(osc->right_phase >= TABLE_SIZE) osc->right_phase = osc->right_phase - TABLE_SIZE;
    }

    osc->frames_played += framesPerBuffer;
    
    // forget about the current note if we finished playing it
    if(osc->frames_played >= osc->num_frames) {
        osc->frames_played = -1; 
        osc->num_frames = 0;
    }

    return paContinue; 
}

void playOsc(int id, int ms, double hz) 
{

    struct note n;
    n.ms = ms; 
    n.hz = hz;
    PaUtil_WriteRingBuffer(&sine_osc.rbuf, &n, 1);
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
    

    /* Initialize the struct osc for callback function */
    void *dataPtr = malloc(sizeof(struct note) * 1024);
    PaUtil_InitializeRingBuffer(&sine_osc.rbuf, sizeof(struct note), 1024, dataPtr);
    sine_osc.table = sine;
    sine_osc.left_phase = 0;
    sine_osc.right_phase = 0;
    sine_osc.frames_played = -1;
    sine_osc.num_frames = -1;
    sine_osc.vol = 0;

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


    /* Register output-only stream callback */
    err = Pa_OpenStream( &stream, NULL, &outputParameters, SAMPLE_RATE, 
                        FRAMES_PER_BUFFER, paNoFlag, paCallback, &sine_osc);
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

