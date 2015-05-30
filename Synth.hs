{-# LANGUAGE ForeignFunctionInterface #-}
module Synth where
import Foreign.C

foreign import ccall "initSynth" initSynth :: IO ()
foreign import ccall "termSynth" termSynth :: IO ()
foreign import ccall "playOsc" playOsc :: CInt -> CInt -> CDouble -> IO ()



-- play a note with a duration value (whole, half, quarter, eigth)
-- The duration of a whole note is defined through wholeNoteDuration, in milliseconds
play :: String -> String -> CInt -> IO ()
play note "w" wholeNoteDuration = playNote note wholeNoteDuration
play note "h" wholeNoteDuration = playNote note (quot wholeNoteDuration 2)
play note "q" wholeNoteDuration = playNote note (quot wholeNoteDuration 4)
play note "e" wholeNoteDuration = playNote note (quot wholeNoteDuration 8)


-- Play a note for a given duration in milliseconds
playNote :: String -> CInt -> IO ()
playNote "C4" duration = playOsc 0 duration 261.63
playNote "D4" duration = playOsc 0 duration 293.66 
playNote "E4" duration = playOsc 0 duration 329.63 
playNote "F4" duration = playOsc 0 duration 349.23 
playNote "G4" duration = playOsc 0 duration 392.00 
playNote "A4" duration = playOsc 0 duration 440.00 
playNote "B4" duration = playOsc 0 duration 493.88 
playNote "C5" duration = playOsc 0 duration 523.25 
playNote "D5" duration = playOsc 0 duration 587.33 
