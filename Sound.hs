{-# LANGUAGE ForeignFunctionInterface #-}
module Sound where
import Foreign.C

foreign import ccall "initPlayer" initPlayer :: IO ()
foreign import ccall "cleanPlayer" cleanPlayer :: IO ()
foreign import ccall "playSin" playSin :: CInt -> CDouble -> IO ()
