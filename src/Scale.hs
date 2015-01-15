module Scale where

import Note

data Scale a     = Scale a a a a a a a       deriving (Eq, Ord, Show)

type IntervalScale = Scale Int
type NoteScale     = Scale Note

toList :: Scale a -> [a]
toList (Scale i ii iii iv v vi vii) = [i, ii, iii, iv, v, vi, vii]


applyScale :: IntervalScale -> Note -> [Note]
applyScale scale note = scanl (flip changePitch ) note (toList scale)

scaleFromNotes :: Note -> Note -> Note -> Note -> Note -> Note -> Note -> IntervalScale
scaleFromNotes a b c d e f g = Scale (distance a b) (distance b c) (distance c d)
                                     (distance d e) (distance e f) (distance f g) (distance g a)


major = scaleFromNotes (Note C Natural)
                       (Note D Natural)
                       (Note E Natural)
                       (Note F Natural)
                       (Note G Natural)
                       (Note A Natural)
                       (Note B Natural)

minor = scaleFromNotes (Note A Natural)
                       (Note B Natural)
                       (Note C Natural)
                       (Note D Natural)
                       (Note E Natural)
                       (Note F Natural)
                       (Note G Natural)
