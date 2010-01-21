-- resynth.hs - resynthesize synthetic sounds by detecting frequencies off the
--     microphone and then playing those frequencies back
import Sound.Alsa
import DSP.Estimation.Frequency.Pisarenko (pisarenko)
import Synthesizer.Plain.Play (monoToInt16)

import Foreign (Ptr, Storable, mallocArray, peekArray)
import Control.Applicative ((<$>))
import Control.Monad (forever, unless, when, mapM_)
import Data.Array (listArray)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

type Frequency = Double
type Volume = Double
type MicState = (Frequency,Volume)

main :: IO ()
main = do
    mv <- micThread 16000 1000
    forever $ do
        (freq,volume) <- readMVar mv
        let
            wave = sineWave rate 0.1 freq
            rate = fromIntegral 44100
        when (volume > 0.3)
            $ monoToInt16 rate wave >> return ()
 
micThread :: Int -> Int -> IO (MVar MicState)
micThread sampleRate samples = do
    let
        source = alsaSoundSource "plughw:0,0" soundFormat
        soundFormat = SoundFmt sampleRate :: SoundFmt Double
    buf <- mallocArray samples
    mv <- newMVar (0,0)
    forkIO $ withSoundSource source $ \handle -> forever $ do
        n <- soundSourceRead source handle buf samples
        rawSound <- peekArray n buf
        let
            volume = (sum $ map abs rawSound) / fromIntegral n
            hz' = pisarenko (listArray (0,n-1) rawSound)
            hz = hz' * 2.82e7 / fromIntegral sampleRate
        if isNaN hz
            then swapMVar mv (0,0)
            else swapMVar mv (hz,volume)
    return mv

sineWave :: Double -> Double -> Double -> [Double]
sineWave rate duration freq = concat
    $ take n
    $ repeat $ map sin [ 0, dt .. 2 * pi ]
    where
        dt = freq * 2 * pi / rate
        n = ceiling $ duration * freq
