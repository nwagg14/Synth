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
        (Synth.play "C4"  "w" 250)
        (Synth.play "E4"  "w" 250)
        (Synth.play "G4"  "w" 250)

phraseB = do
        (Synth.play "D4"  "w" 250)
        (Synth.play "F4"  "w" 250)
        (Synth.play "A4"  "w" 250)
