module Audio where

import Control.Monad.Trans.Reader
import Data.Functor
import Data.List (transpose)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Sound.Wav
import Sound.Wav.ChannelData
import Chord

renderChord ::  Int -> Chord -> Reader WaveFormat WaveFile
renderChord len chord = createWave =<< mixTones <$> mapM (pureTone len) (chordToFrequencies chord)

pureTone ::  Int ->  Double -> Reader WaveFormat ([Double])
pureTone len freq  = do
    conf <- ask
    take (len * ( fromIntegral $ waveSampleRate conf )) <$> generateInfiniteSine freq

toFloatingWaveData :: [Double] -> FloatingWaveData
toFloatingWaveData rawData = FloatingWaveData $ [V.fromList rawData]

mixTones :: [[Double]] -> [Double]
mixTones ts = map sum $ transpose $ fmap (fmap  (/ fromIntegral (length ts) )) ts

createWave :: [Double] -> Reader WaveFormat WaveFile
createWave vs = do
    format <- ask
    return $ (flip encodeFloatingWaveData) (toFloatingWaveData vs)
           WaveFile { waveFormat = format
                    , waveData = BL.empty
                    , waveFact = Nothing
                    , waveInfo = Just $ waveInfoDefault { creationSoftware = Just "wavy (Sine Generate)" } }

generateInfiniteSine :: Double -> Reader WaveFormat [Double]
generateInfiniteSine freq = mapM (generateValue freq) [1.0..]

generateValue :: Double -> Double -> Reader WaveFormat Double
generateValue freq count = do
    conf <- ask
    return $ 0.6 * sin (twoPI * count * freq / fromIntegral (waveSampleRate conf) )

twoPI :: Floating a => a
twoPI = pi * 2
