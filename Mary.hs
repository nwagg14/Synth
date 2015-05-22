module Mary where
import qualified Sound

main = do
        (Sound.initPlayer)
        (maryHadALamb 1500)
        (Sound.cleanPlayer)

maryHadALamb wholeDur = do
        (mary   wholeDur)
        (lambA  wholeDur)
        (lambB  wholeDur)
        (lambC  wholeDur)
        (mary   wholeDur)
        (lambD  wholeDur)
        (fleece wholeDur)
        (snow   wholeDur)

mary wholeDur = do
        (Sound.playNote "B4"  "q" wholeDur)
        (Sound.playNote "A4"  "q" wholeDur)
        (Sound.playNote "G4"  "q" wholeDur)
        (Sound.playNote "A4"  "q" wholeDur)

lambA wholeDur = do
        (Sound.playNote "B4"  "q" wholeDur)
        (Sound.playNote "B4"  "q" wholeDur)
        (Sound.playNote "B4"  "h" wholeDur)
    
lambB wholeDur = do
        (Sound.playNote "A4"  "q" wholeDur)
        (Sound.playNote "A4"  "q" wholeDur)
        (Sound.playNote "A4"  "h" wholeDur)

lambC wholeDur = do
        (Sound.playNote "B4"  "q" wholeDur)
        (Sound.playNote "D5"  "q" wholeDur)
        (Sound.playNote "D5"  "h" wholeDur)

lambD wholeDur = do
        (Sound.playNote "B4"  "q" wholeDur)
        (Sound.playNote "B4"  "q" wholeDur)
        (Sound.playNote "B4"  "q" wholeDur)
        (Sound.playNote "B4"  "q" wholeDur)

fleece wholeDur = do
        (Sound.playNote "A4"  "q" wholeDur)
        (Sound.playNote "A4"  "q" wholeDur)
        (Sound.playNote "B4"  "q" wholeDur)
        (Sound.playNote "A4"  "q" wholeDur)

snow wholeDur = do
        (Sound.playNote "G4"  "w" wholeDur)
