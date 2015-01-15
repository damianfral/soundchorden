module Chord where

import Control.Applicative
import Control.Monad.Trans.Reader

import Data.Functor
import Data.List
import Data.Maybe

import Data.Int

import Note
import Scale

data Grade       = I | II | III | IV | V | VI | VII deriving (Eq, Ord, Show, Enum)
data Chord       = Chord Note [Grade] IntervalScale  deriving (Eq, Ord, Show)

-- 2 notes
_powerchord = [I, V]

-- 3 notes
_triad      = [I, III, V]
_sus2       = [I, II , V]
_sus4       = [I, IV , V]

-- 4 notes
_sixth      = [I, III, V, VI]
_6sus2      = [I, II , V, VI]
_6sus4      = [I, IV , V, VI]

_seventh    = [I, III, V, VII]
_7sus2      = [I, II , V, VII]
_7sus4      = [I, IV , V, VII]

_add9       = [I, IV , V, II]
_add11      = [I, IV , V, IV]

-- 5 notes
_nineth     = [I, III, V, VII, II]
_6add9      = [I, III, V, VI , II]
_9sus4      = [I, IV , V, VII, II]
_7add13     = [I, III, V, VII, VI]

chordToNotes :: Chord -> [Note]
chordToNotes (Chord note grades scale) = map (\g -> notes !! fromEnum g) grades
    where notes = applyScale scale note

chordToFrequencies chord = fs ++ octaves1 ++ octaves2
    where
        fs = map noteToFrequency $ chordToNotes chord
        octaves1 = (*2) <$> fs
        octaves2 = (*3) <$> fs
