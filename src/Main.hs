module Main where

import Control.Monad.Trans.Reader

import System.Environment
import Sound.Wav
import Audio
import Note
import Scale
import Chord

main :: IO ()
main = do
    (filename:_) <- getArgs
    putStrLn $ "Writting " ++ filename
    encodeWaveFile filename $ flip runReader format $ renderChord 4 chord
    where
        chosenSampleRate = 48000
        format :: WaveFormat
        format = WaveFormat
               { waveAudioFormat    = MicrosoftPCM
               , waveNumChannels    = 1
               , waveSampleRate     = chosenSampleRate
               , waveByteRate       = chosenSampleRate * 2
               , waveBlockAlignment = 1
               , waveBitsPerSample  = 16 }
        chord = Chord (Note F Sharp) _triad major
