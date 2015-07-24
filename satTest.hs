size = 4
wumpus = (0,2)
pits = [(2, 0)]
rangeError x y = x < 0 || y < 0 || x >= size || y >= size

-- Ob es auf dem Feld x y nach Wumpus riecht.
smellAt x y = 	(x-1,y) == wumpus ||
				(x+1,y) == wumpus ||
				(x,y-1) == wumpus ||
				(x,y+1) == wumpus

--filter' [] = []
--filter' (h:t) = [  ]
filterOr [] = []
filterOr (h:t) = [[e | e <- h, not $ null e ]] ++ filterOr t
filterAnd xs = [e | e <- xs, not $ null e]
filter' xs = filterAnd $ filterOr xs



-- Bastelt den Atomnamen zusammen aus der ID und den Koordinaten. 
-- Unter Umständen negiert.
calcAtomAt id x y neg 
	| rangeError x y = []
	| otherwise = getNeg neg ++ id ++ "_" ++ show x ++ "_" ++ show y
		where 	getNeg neg 
				| neg = ""
				| otherwise = "-"

-- Gleich wie calcAtomAt, nur mit einem Zeitstempel angehängt.
calcTimeAtomAt id x y t neg
	| rangeError x y = []
	| otherwise = getNeg neg ++ id ++ "_" ++ show x ++ "_" ++ show y ++ "_" ++ show t
		where 	getNeg neg
				| neg = ""
				| otherwise = "-"

-- Ob man eine Briese auf dem Feld spürt.
breezeAt x y = 	elem (x-1,y) pits ||
				elem (x+1,y) pits ||
				elem (x,y-1) pits ||
				elem (x,y+1) pits

-- Bastelt einen logischen Pit-Ausdruck zusammen für eine Position.
-- --> Eventuell die Reihenfolge tauschen oder gucken, ob beide Fälle zutreffen (im Pit + Breeze!) 
-- Problem wäre gelöst, wenn der Fall nie eintritt, dass der Agent auf ein Pit läuft.
pitSense x y
	{-- Evtl. den Fall wegoptimieren, dass der Agent nie in ein Pit selber läuft... --}
	| elem (x,y) pits = filter' $	[[calcAtomAt "P" x y True]]
	| breezeAt x y = filter' $ 	[[calcAtomAt "P" x y False]] ++
								[	[calcAtomAt "P" (x-1) y True] ++ 
									[calcAtomAt "P" (x+1) y True] ++
									[calcAtomAt "P" x (y-1) True] ++
									[calcAtomAt "P" x (y+1) True]
								]
	| otherwise = filter' $ [[calcAtomAt "P" x y False]] ++
							[[calcAtomAt "P" (x-1) y False]] ++
							[[calcAtomAt "P" (x+1) y False]] ++
							[[calcAtomAt "P" x (y-1) False]] ++
							[[calcAtomAt "P" x (y+1) False]]
						
