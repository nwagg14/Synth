#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "portaudio.h"
#include "pa_ringbuffer.h"
#include "synth.h"

#define SAMPLE_RATE (44100)
#define FRAMES_PER_BUFFER (4410)
#define TABLE_SIZE (210)
#define NUM_OSCILLATORS (1)

enum note_type {
    NOTE,       // a note with a millisecond duration (ms) and frequency (hz)
    REST,       // a note with a millisecont duration (ms) but no sound
    WAITING,    // signifies that no note was read from the RingBuffer
    END         // signals that this oscillator is done being used
};

struct frame {
    float left;
    float right;
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

/* Globals */
float sine[TABLE_SIZE];
PaStream *stream;
struct osc oscillators[NUM_OSCILLATORS];

/* Function declarations */
// print an error and abort
void error(PaError err);

// callback used by PulseAudio to generate sound
int paCallback(const void *inputBuffer, void *outputBuffer,
                unsigned long framesPerBuffer,
                const PaStreamCallbackTimeInfo *timeInfo,
                PaStreamCallbackFlags statusFlags,
                void *userData);

struct frame getNextFrame(struct osc *osc);
 
// registers a note on the given oscillator at hz frequency for ms milliseconds
void playOsc(unsigned int id, unsigned int ms, double hz);

// registers a rest(no sound) on the given oscillator for ms milliseconds
void restOsc(unsigned int id, unsigned int ms);

// signals that this oscillator is done being used.
void endOsc(unsigned int id);

// initializes Synth
void initSynth(void);
// terminates Synth
void termSynth(void);

// helper function for initSynth. Initializes the oscillators array
void initOscillators(void);

/* Function definitions */

void error(PaError err)
{
    printf("Error message: %s\n", Pa_GetErrorText(err));
    exit(-1);
}


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
            osc->curr_note.ms = 0;
            osc->curr_note.hz = 0;
            osc->curr_note.type = WAITING; 
        }
        
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

        struct frame curr_frame = getNextFrame(osc);
        *buffer++ = curr_frame.left; 
        *buffer++ = curr_frame.right;
    }

    osc->frames_played += framesPerBuffer;
    
    // forget about the current note if we finished playing it
    if(osc->frames_played >= osc->num_frames) {
        osc->frames_played = -1; 
        osc->num_frames = 0;
    }

    return paContinue; 
}

struct frame getNextFrame(struct osc *osc) {

    // build the return value
    struct frame f;
    if(osc->curr_note.type == REST || 
       osc->curr_note.type == WAITING || 
       osc->curr_note.type == END) {
        f.left = 0;
        f.right = 0;
    }

    // note must be of type NOTE (or something crazy)
    else {
        f.left = osc->table[(int)osc->left_phase]; 
        f.right = osc->table[(int)osc->right_phase]; 
    
        // update lookup table indices
        osc->left_phase = (osc->left_phase + osc->curr_note.hz/TABLE_SIZE);
        osc->right_phase = (osc->right_phase + osc->curr_note.hz/TABLE_SIZE);
        if(osc->left_phase >= TABLE_SIZE)
            osc->left_phase = osc->left_phase - TABLE_SIZE;
        if(osc->right_phase >= TABLE_SIZE)
            osc->right_phase = osc->right_phase - TABLE_SIZE;
    }

    return f;
}

void playOsc(unsigned int id, unsigned int ms, double hz) 
{

    if(id >= NUM_OSCILLATORS) {
        printf("id %i is too large.\n", id);
        return;
    }
    
    struct note n;
    n.type = NOTE;
    n.ms = ms; 
    n.hz = hz;
    PaUtil_WriteRingBuffer(&oscillators[id].rbuf, &n, 1);
}

void restOsc(unsigned int id, unsigned int ms)
{
    if(id >= NUM_OSCILLATORS) {
        printf("id %i is too large.\n", id);
        return;
    }
    
    struct note n;
    n.type = REST;
    n.ms = ms; 
    n.hz = 0;
    PaUtil_WriteRingBuffer(&oscillators[id].rbuf, &n, 1);
}

void endOsc(unsigned int id) 
{
    if(id >= NUM_OSCILLATORS) {
        printf("id %i is too large.\n", id);
        return;
    }
    
    struct note n;
    n.type = END;
    n.ms = 0; 
    n.hz = 0;
    PaUtil_WriteRingBuffer(&oscillators[id].rbuf, &n, 1);
}

void initOscillators(void)
{
    int i;
    for(i = 0; i < NUM_OSCILLATORS; i++) {
        /* Initialize the struct osc for callback function */
        void *dataPtr = malloc(sizeof(struct note) * 1024);
        PaUtil_InitializeRingBuffer(&oscillators[i].rbuf, sizeof(struct note), 1024, dataPtr);
        oscillators[i].table = sine;
        oscillators[i].left_phase = 0;
        oscillators[i].right_phase = 0;
        oscillators[i].frames_played = -1;
        oscillators[i].num_frames = -1;
        oscillators[i].vol = 0;
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
    
    initOscillators();

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
                        FRAMES_PER_BUFFER, paNoFlag, paCallback, &oscillators);
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

