module Utils where
    
_split :: Char -> String -> String -> [String]
_split c (x : xs) accum = if c == x
    then accum : _split c xs []
    else _split c xs (accum ++ [x])

_split _ [] accum = [accum]

split :: Char -> String -> [String]
split c xs = _split c xs []

takeUntil :: [a] -> (a -> Bool) -> ([a], [a])
takeUntil (a : as) fn = if fn a
    then ([], a : as)
    else do
        let (a1, a2) = takeUntil as fn
        (a : a1, a2)
takeUntil [] _ = ([], [])