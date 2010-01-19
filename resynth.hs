-- resynth.hs - resynthesize synthetic sounds by detecting frequencies off the
--     microphone and then playing those frequencies back
import Sound.Alsa
import DSP.Estimation.Frequency.Pisarenko (pisarenko)
import Synthesizer.Plain.Play (monoToInt16)

import Foreign (Ptr, Storable, mallocArray, peekArray)
import Control.Applicative ((<$>))
import Control.Monad (forever, unless, when)
import Data.Array (listArray)

import Control.Concurrent (forkIO)

main :: IO ()
main = do
    let sampleRate = 44100
    micFreq 8000 1000 $ \freq volume -> do
        when (volume > 2.1e-5) $ do
            forkIO $ playFreq 44100 0.25 freq
            print (freq, volume)

micFreq :: Int -> Int -> (Double -> Double -> IO ()) -> IO ()
micFreq sampleRate samples handler = do
    let
        source = alsaSoundSource "plughw:0,0" soundFormat
        soundFormat = SoundFmt sampleRate :: SoundFmt Double
    buf <- mallocArray samples
    
    withSoundSource source $ \handle -> forever $ do
        n <- soundSourceRead source handle buf samples
        rawSound <- peekArray n buf
        
        let
            volume = (sum $ map abs rawSound) / fromIntegral n
            hz' = pisarenko (listArray (0,n-1) rawSound)
            hz = hz' * 90 * (fromIntegral sampleRate) / (fromIntegral samples)
        
        unless (isNaN hz) $ handler hz volume

playFreq :: Int -> Double -> Double -> IO ()
playFreq sampleRate duration freq =
    monoToInt16 rate sineWave >> return ()
    where
        sineWave = take (floor $ duration * rate)
            $ map sin [ 0, freq * 2 * pi / rate ..]
        rate = fromIntegral sampleRate
