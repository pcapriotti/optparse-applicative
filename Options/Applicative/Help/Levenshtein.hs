module Options.Applicative.Help.Levenshtein (
    editDistance
  ) where

-- | Calculate the Damerau-Levenshtein edit distance
--   between two lists (strings).
--
--   Optparse can't really take on any dependencies
--   so we're bringing it in here.
--
--   This is modified from
--   https://wiki.haskell.org/Edit_distance
--   and is originally from Lloyd Allison's paper
--   "Lazy Dynamic-Programming can be Eager"
--
--   It's been changed though from Levenshtein to
--   Damerau-Levenshtein, which treats transposition
--   of adjacent characters as one change instead of
--   two.
--
--   The significant difference is an extra case to
--   doDiag, which checks if it's actually a
--   transposition.
--
--   As there are a few ugly partial function calls
--   there's property tests to ensure it doesn't
--   crash :/ and obeys the laws.
--
editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b
    = last (if lab == 0 then mainDiag
            else if lab > 0 then lowers !! (lab - 1)
                 else {- < 0 -}  uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag _ [] _ = []
          eachDiag _ _ [] = []
          eachDiag a' (_:bs) (lastDiag:diags) = oneDiag a' bs nextDiag lastDiag : eachDiag a' bs diags
              where nextDiag = head (tail diags)
          oneDiag a' b' diagAbove diagBelow = thisdiag
              where doDiag [] _ _ _ _ = []
                    doDiag _ [] _ _ _ = []
                    -- Check for a transposition
                    doDiag (ach:ach':as) (bch:bch':bs) nw n w
                      | ach' == bch && ach == bch'
                      = nw : (doDiag (ach':as) (bch':bs) nw (tail n) (tail w))
                    -- Usual case
                    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a' b' firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z
