import Sound.Alsa
import Foreign (Ptr, Storable, mallocArray, peekArray, Int16)
import Control.Monad (forever)
import Control.Applicative ((<$>))
import Data.Array (listArray)
import DSP.Estimation.Frequency.Pisarenko (pisarenko)

soundFormat :: SoundFmt Float
soundFormat = SoundFmt 8000

main :: IO ()
main = do
    let source = alsaSoundSource "plughw:0,0" soundFormat
    let bufSize = 10 ^ 2
    buf <- mallocArray bufSize :: IO (Ptr Float)
    
    withSoundSource source $ \handle -> forever $ do
        n <- soundSourceRead source handle buf bufSize
        rawSound <- listArray (0,n-1) <$> peekArray n buf
        let hz = 100 * pisarenko rawSound
        putStrLn $ show hz ++ " Hz"
