-- **********************************************************************

-- Exercise 1

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail [] = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Nothing
safeInit (x:xs) = Just (x : loop xs)
	where
		loop [] = []
		loop (x:[]) = []
		loop (x:xs) = x : loop xs

-- **********************************************************************

-- Exercise 2

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p xs =
	let (as, bs) = break (not . p) xs
	in as : case bs of
		[] -> []
		_ -> splitWith p (snd $ span (not . p) bs)

-- **********************************************************************
