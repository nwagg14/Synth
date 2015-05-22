{-# LANGUAGE ForeignFunctionInterface #-}
module Sound where
import Foreign.C
import Control.Exception

foreign import ccall "initPlayer" initPlayer :: IO ()
foreign import ccall "cleanPlayer" cleanPlayer :: IO ()
foreign import ccall "playSin" playSin :: CInt -> CDouble -> IO ()

main = do
        (initPlayer)
        (phraseA)
        (phraseA)
        (phraseA)
        (phraseA)
        (phraseB)
        (phraseB)
        (phraseB)
        (phraseB)
        (cleanPlayer)

phraseA = do
       (playSin 250 261.63)
       (playSin 250 329.63)
       (playSin 250 392)

phraseB = do
       (playSin 250 293.66)
       (playSin 250 349.23)
       (playSin 250 440)
