-- resynth.hs - resynthesize synthetic sounds by detecting frequencies off the
--     microphone and then playing those frequencies back
import Sound.Alsa
import DSP.Estimation.Frequency.Pisarenko (pisarenko)
import Synthesizer.Plain.Play (monoToInt16)

import Foreign (Ptr, Storable, mallocArray, peekArray)
import Control.Applicative ((<$>))
import Control.Monad (forever, unless, when, forM_)
import Data.Array (listArray)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan

main :: IO ()
main = do
    fx <- mic 16000 2000
    forM_ fx $ \(freq,volume) -> do
        print (freq,volume)
    
    {-
    when (volume > 0.3) $ do
        forkIO $ playFreq 44100 0.1 freq
        print (freq, volume)
    -}

mic :: Int -> Int -> IO [(Double,Double)]
mic sampleRate samples = do
    let
        source = alsaSoundSource "plughw:0,0" soundFormat
        soundFormat = SoundFmt sampleRate :: SoundFmt Double
    buf <- mallocArray samples
    
    -- spawn and populate a channel
    chan <- newChan
    forkIO $ withSoundSource source $ \handle -> forever $ do
        n <- soundSourceRead source handle buf samples
        rawSound <- peekArray n buf
        let
            volume = (sum $ map abs rawSound) / fromIntegral n
            hz' = pisarenko (listArray (0,n-1) rawSound)
            hz = hz' * 2.82e7 / fromIntegral sampleRate
        unless (isNaN hz) $ writeChan chan (hz,volume)
    
    -- lazily read back everything from the channel
    getChanContents chan

playFreqs :: Int -> Double -> [Double] -> IO ()
playFreqs sampleRate duration freqs =
    monoToInt16 rate waves >> return ()
    where
        waves = concatMap (sineWave rate duration) freqs
        rate = fromIntegral sampleRate

sineWave :: Double -> Double -> Double -> [Double]
sineWave rate duration freq = concat
    $ take n
    $ repeat $ map sin [ 0, dt .. 2 * pi ]
    where
        dt = freq * 2 * pi / rate
        n = ceiling $ duration * freq
