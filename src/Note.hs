module Note where

import Data.Functor

data Key         = C | D | E | F | G | A | B deriving (Eq, Ord, Show, Enum)
data Alteration  = Flat | Natural | Sharp    deriving (Eq, Ord, Show, Enum)
data Note        = Note Key Alteration       deriving (Eq, Ord, Show)

-- same row for equivalent notes
allNotes = [ Note C Natural , Note B Sharp   ]
         : [ Note C Sharp   , Note D Flat    ]
         : [ Note D Natural                  ]
         : [ Note D Sharp   , Note E Flat    ]
         : [ Note E Natural , Note F Flat    ]
         : [ Note F Natural , Note E Sharp   ]
         : [ Note F Sharp   , Note G Flat    ]
         : [ Note G Natural                  ]
         : [ Note G Sharp   , Note A Flat    ]
         : [ Note A Natural                  ]
         : [ Note A Sharp   , Note B Flat    ]
         : [ Note B Natural , Note C Flat    ]
         : allNotes

allNotesFrom n = dropWhile (notElem n) allNotes

-- from a to b
distance :: Note -> Note -> Int
distance a b = index b (allNotesFrom a) `mod` 12
    where index x (n:ns) | x `elem` n = 0
                         | otherwise  = 1 + index x ns

changePitch :: Int -> Note -> Note
changePitch x note | x < 0     = changePitch (12 + x) note
                   | otherwise = (head <$> allNotesFrom note) !! x

noteToFrequency :: Note -> Double
noteToFrequency x  = 110 * 2 ** (fromIntegral (distance (Note A Natural) x) / 12)
