module Example where
import qualified Synth

main = do
        (Synth.initSynth)
        (loop majorPhrase 2)
        (Synth.termSynth)

loop f 1 = do (f)
loop f n = do
            (f)
            (loop f (n-1))



majorPhrase = do
        (loop phraseA 4)
        (loop phraseB 4)
phraseA = do
        (Synth.playNote "C4"  "w" 250)
        (Synth.playNote "E4"  "w" 250)
        (Synth.playNote "G4"  "w" 250)

phraseB = do
        (Synth.playNote "D4"  "w" 250)
        (Synth.playNote "F4"  "w" 250)
        (Synth.playNote "A4"  "w" 250)
