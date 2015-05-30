module Mary where
import qualified Synth

main = do
        (Synth.initSynth)
        (maryHadALamb 1500)
        name <- getLine
        (Synth.termSynth)

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
        (Synth.playNote "B4"  "q" wholeDur)
        (Synth.playNote "A4"  "q" wholeDur)
        (Synth.playNote "G4"  "q" wholeDur)
        (Synth.playNote "A4"  "q" wholeDur)

lambA wholeDur = do
        (Synth.playNote "B4"  "q" wholeDur)
        (Synth.playNote "B4"  "q" wholeDur)
        (Synth.playNote "B4"  "h" wholeDur)
    
lambB wholeDur = do
        (Synth.playNote "A4"  "q" wholeDur)
        (Synth.playNote "A4"  "q" wholeDur)
        (Synth.playNote "A4"  "h" wholeDur)

lambC wholeDur = do
        (Synth.playNote "B4"  "q" wholeDur)
        (Synth.playNote "D5"  "q" wholeDur)
        (Synth.playNote "D5"  "h" wholeDur)

lambD wholeDur = do
        (Synth.playNote "B4"  "q" wholeDur)
        (Synth.playNote "B4"  "q" wholeDur)
        (Synth.playNote "B4"  "q" wholeDur)
        (Synth.playNote "B4"  "q" wholeDur)

fleece wholeDur = do
        (Synth.playNote "A4"  "q" wholeDur)
        (Synth.playNote "A4"  "q" wholeDur)
        (Synth.playNote "B4"  "q" wholeDur)
        (Synth.playNote "A4"  "q" wholeDur)

snow wholeDur = do
        (Synth.playNote "G4"  "w" wholeDur)
