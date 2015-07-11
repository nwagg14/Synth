module Mary where
import qualified Synth

main = do
        (Synth.initSynth)
        (maryHadALamb 1500)
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
        (Synth.endOsc 0)
        (Synth.endOsc 1)

mary wholeDur = do
        (Synth.play "B4"  "q" wholeDur)
        (Synth.play "A4"  "q" wholeDur)
        (Synth.play "G4"  "q" wholeDur)
        (Synth.play "A4"  "q" wholeDur)

lambA wholeDur = do
        (Synth.play "B4"  "q" wholeDur)
        (Synth.play "B4"  "q" wholeDur)
        (Synth.play "B4"  "h" wholeDur)
    
lambB wholeDur = do
        (Synth.play "A4"  "q" wholeDur)
        (Synth.play "A4"  "q" wholeDur)
        (Synth.play "A4"  "h" wholeDur)

lambC wholeDur = do
        (Synth.play "B4"  "q" wholeDur)
        (Synth.play "D5"  "q" wholeDur)
        (Synth.play "D5"  "h" wholeDur)

lambD wholeDur = do
        (Synth.play "B4"  "q" wholeDur)
        (Synth.play "B4"  "q" wholeDur)
        (Synth.play "B4"  "q" wholeDur)
        (Synth.play "B4"  "q" wholeDur)

fleece wholeDur = do
        (Synth.play "A4"  "q" wholeDur)
        (Synth.play "A4"  "q" wholeDur)
        (Synth.play "B4"  "q" wholeDur)
        (Synth.play "A4"  "q" wholeDur)

snow wholeDur = do
        (Synth.play "G4"  "w" wholeDur)
