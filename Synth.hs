{-# LANGUAGE ForeignFunctionInterface #-}
module Synth where
import Foreign.C

foreign import ccall "initSynth" initSynth :: IO ()
foreign import ccall "termSynth" termSynth :: IO ()
foreign import ccall "playSin" playSin :: CInt -> CDouble -> IO ()



-- play a note with a duration value (whole, half, quarter, eigth)
-- The duration of a whole note is defined through wholeNoteDuration, in milliseconds
playNote :: String -> String -> CInt -> IO ()
playNote note "w" wholeNoteDuration = play note wholeNoteDuration
playNote note "h" wholeNoteDuration = play note (quot wholeNoteDuration 2)
playNote note "q" wholeNoteDuration = play note (quot wholeNoteDuration 4)
playNote note "e" wholeNoteDuration = play note (quot wholeNoteDuration 8)


-- Play a note for a given duration in milliseconds
play :: String -> CInt -> IO ()
play "C4" duration = playSin duration 261.63
play "D4" duration = playSin duration 293.66 
play "E4" duration = playSin duration 329.63 
play "F4" duration = playSin duration 349.23 
play "G4" duration = playSin duration 392.00 
play "A4" duration = playSin duration 440.00 
play "B4" duration = playSin duration 493.88 
play "C5" duration = playSin duration 523.25 
play "D5" duration = playSin duration 587.33 