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
       (Sound.playSin 250 261.63)
       (Sound.playSin 250 329.63)
       (Sound.playSin 250 392)

phraseB = do
       (Sound.playSin 250 293.66)
       (Sound.playSin 250 349.23)
       (Sound.playSin 250 440)
