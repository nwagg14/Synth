module Example where
import qualified Sound

main = do
        (Sound.initPlayer)
        (loop majorPhrase 2)
        (Sound.cleanPlayer)

loop f 1 = do (f)
loop f n = do
            (f)
            (loop f (n-1))



majorPhrase = do
        (loop phraseA 4)
        (loop phraseB 4)
phraseA = do
        (Sound.playNote "C4"  "w" 250)
        (Sound.playNote "E4"  "w" 250)
        (Sound.playNote "G4"  "w" 250)

phraseB = do
        (Sound.playNote "D4"  "w" 250)
        (Sound.playNote "F4"  "w" 250)
        (Sound.playNote "A4"  "w" 250)
