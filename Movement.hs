module Movement (calcMovement, sideLeft') where
import Cnf
import GeneralLogic

-- Findet die erste Position eines Zeichens in einem String.
findChar _ [] = 0
findChar c (h:t) 
	| c == h = 0
	| otherwise = 1 + findChar c t

-- Erstellt ein Tupel mit dem Anfang, bis zu einem Trennzeichen c und dem Rest
splitOn' c s = splitAt (1 + findChar c s) s
splitOn c s = ([s | s <- fst $ splitOn' c s, s /= c, s /= ' '], [s | s <- snd $ splitOn' c s, s /= ' '])

-- Erstellt eine And-Verknüpfung.
parseAnd' (('-':f),"") =  (Not (At f))
parseAnd' (f,"") =  (At f)
parseAnd' (('-':f),s) = (And (Not (At f)) (parseAnd' $ splitOn '&' s))
parseAnd' (f,s) = (And (At f) (parseAnd' $ splitOn '&' s))

-- And-Verknüpfung von Atomen. Z.B. (L_x+1_y_t-1 & West_t-1)
parseAnd s = parseAnd' $ splitOn '&' s

{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x+1_y_t-1 & West_t-1) |
                 (L_x-1_y_t-1 & East_t-1) |
                 (L_x_y-1_t-1 & North_t-1) |
	             (L_x_y+1_t-1 & South_t-1))
--}
normalMovement x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x+1) y (t-1) True) (At $ calcDirAtomAt "West" (t-1) True)) 
	(Or (And (At $ calcTimeAtomAt "L" (x-1) y (t-1) True) (At $ calcDirAtomAt "East" (t-1) True)) 
	(Or (And (At $ calcTimeAtomAt "L" x (y-1) (t-1) True) (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (At $ calcTimeAtomAt "L" x (y+1) (t-1) True) (At $ calcDirAtomAt "South" (t-1) True)))))))

{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x+1_y_t-1 & West_t-1) |
	             (L_x_y+1_t-1 & South_t-1))
--}
cornerDownLeft  x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x+1) y (t-1) True) (At $ calcDirAtomAt "West" (t-1) True)) 
		(And (At $ calcTimeAtomAt "L" x (y+1) (t-1) True) (At $ calcDirAtomAt "South" (t-1) True)))))

{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x+1_y_t-1 & West_t-1) |
                 (L_x_y-1_t-1 & North_t-1)
--}	
cornerUpLeft    x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x+1) y (t-1) True) (At $ calcDirAtomAt "West" (t-1) True))
		(And (At $ calcTimeAtomAt "L" x (y-1) (t-1) True) (At $ calcDirAtomAt "North" (t-1) True)))))

{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x-1_y_t-1 & East_t-1) |
	             (L_x_y+1_t-1 & South_t-1))
--}
cornerDownRight x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x-1) y (t-1) True) (At $ calcDirAtomAt "East" (t-1) True)) 
		(And (At $ calcTimeAtomAt "L" x (y+1) (t-1) True) (At $ calcDirAtomAt "South" (t-1) True)))))

{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x-1_y_t-1 & East_t-1) |
                 (L_x_y-1_t-1 & North_t-1)
--}
cornerUpRight   x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x-1) y (t-1) True) (At $ calcDirAtomAt "East" (t-1) True)) 
		(And (At $ calcTimeAtomAt "L" x (y-1) (t-1) True) (At $ calcDirAtomAt "North" (t-1) True)))))

{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x+1_y_t-1 & West_t-1) |
                 (L_x_y-1_t-1 & North_t-1) |
	             (L_x_y+1_t-1 & South_t-1))
--}
sideLeft x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x+1) y (t-1) True) (At $ calcDirAtomAt "West" (t-1) True))  
	(Or (And (At $ calcTimeAtomAt "L" x (y-1) (t-1) True) (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (At $ calcTimeAtomAt "L" x (y+1) (t-1) True) (At $ calcDirAtomAt "South" (t-1) True))))))
		
{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x+1_y_t-1 & West_t-1) |
                 (L_x_y-1_t-1 & North_t-1) |
	             (L_x_y+1_t-1 & South_t-1))
--}		
sideLeft' x y t = convertToCnfList (Cond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (parseAnd $ calcTimeAtomAt "L" x y (t-1) True ++ 
		" & " ++ calcDirAtomAt "North" (t-1) False ++ 
		" & " ++ calcDirAtomAt "South" (t-1) False ++ 
		" & " ++ calcDirAtomAt "West"  (t-1) False ++ 
		" & " ++ calcDirAtomAt "East"  (t-1) False 
		--" & " ++ calcTimeAtomAt "L" (x-1) y (t-1) False ++
		--" & " ++ calcTimeAtomAt "L" (x+1) y (t-1) False ++
		--" & " ++ calcTimeAtomAt "L" x (y-1) (t-1) False ++
		--" & " ++ calcTimeAtomAt "L" x (y+1) (t-1) False
		) 
	(Or (parseAnd (calcTimeAtomAt "L" (x+1) y (t-1) True ++ 
		" & " ++ calcDirAtomAt "West" (t-1) True
		--" & " ++ calcDirAtomAt "East" (t-1) False ++
		--" & " ++ calcDirAtomAt "South" (t-1) False ++
		--" & " ++ calcDirAtomAt "North" (t-1) False ++
		--" & " ++ calcTimeAtomAt "L" (x-1) y (t-1) False ++
		--" & " ++ calcTimeAtomAt "L" x (y-1) (t-1) False ++
		--" & " ++ calcTimeAtomAt "L" x (y+1) (t-1) False
		))
	(Or (parseAnd (calcTimeAtomAt "L" x (y-1) (t-1) True ++ 
		" & " ++ calcDirAtomAt "North" (t-1) True ++
		" & " ++ calcDirAtomAt "West" (t-1) False ++
		" & " ++ calcDirAtomAt "South" (t-1) False ++
		" & " ++ calcDirAtomAt "East" (t-1) False ++
		" & " ++ calcTimeAtomAt "L" (x+1) y (t-1) False ++
		" & " ++ calcTimeAtomAt "L" (x-1) y (t-1) False ++
		" & " ++ calcTimeAtomAt "L" x (y+1) (t-1) False))
		(parseAnd (calcTimeAtomAt "L" x (y+1) (t-1) True ++ 
		" & " ++ calcDirAtomAt "South" (t-1) True ++
		" & " ++ calcDirAtomAt "West" (t-1) False ++
		" & " ++ calcDirAtomAt "North" (t-1) False ++
		" & " ++ calcDirAtomAt "East" (t-1) False ++
		" & " ++ calcTimeAtomAt "L" (x+1) y (t-1) False ++
		" & " ++ calcTimeAtomAt "L" (x-1) y (t-1) False ++
		" & " ++ calcTimeAtomAt "L" x (y-1) (t-1) False))
	)))
	)

{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x+1_y_t-1 & West_t-1) |
                 (L_x-1_y_t-1 & East_t-1) |
	             (L_x_y+1_t-1 & South_t-1))
--}
sideDown x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x+1) y (t-1) True) (At $ calcDirAtomAt "West" (t-1) True)) 
	(Or (And (At $ calcTimeAtomAt "L" (x-1) y (t-1) True) (At $ calcDirAtomAt "East" (t-1) True)) 
		(And (At $ calcTimeAtomAt "L" x (y+1) (t-1) True) (At $ calcDirAtomAt "South" (t-1) True))))))
		
{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x-1_y_t-1 & East_t-1) |
                 (L_x_y-1_t-1 & North_t-1) |
	             (L_x_y+1_t-1 & South_t-1))
--}
sideRight x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x-1) y (t-1) True) (At $ calcDirAtomAt "East" (t-1) True)) 
	(Or (And (At $ calcTimeAtomAt "L" x (y-1) (t-1) True) (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (At $ calcTimeAtomAt "L" x (y+1) (t-1) True) (At $ calcDirAtomAt "South" (t-1) True))))))

{--
	L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
                  ~West_t-1 & ~East_t-1) |
                 (L_x+1_y_t-1 & West_t-1) |
                 (L_x-1_y_t-1 & East_t-1) |
                 (L_x_y-1_t-1 & North_t-1)
--}
sideUp x y t = convertToCnfList (Bicond (At $ calcTimeAtomAt "L" x y t True) 
	(Or (And (At $ calcTimeAtomAt "L" x y (t-1) True) 
		(And (Not (At $ calcDirAtomAt "North" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "South" (t-1) True)) 
		(And (Not (At $ calcDirAtomAt "West"  (t-1) True)) 
			 (Not (At $ calcDirAtomAt "East"  (t-1) True)))))) 
	(Or (And (At $ calcTimeAtomAt "L" (x+1) y (t-1) True) (At $ calcDirAtomAt "West" (t-1) True)) 
	(Or (And (At $ calcTimeAtomAt "L" (x-1) y (t-1) True) (At $ calcDirAtomAt "East" (t-1) True)) 
		(And (At $ calcTimeAtomAt "L" x (y-1) (t-1) True) (At $ calcDirAtomAt "North" (t-1) True))))))

-- Bastelt einen logischen Bewegungs-Ausdruck zusammen für eine Position
calcMovement x y t 
	-- Ecken: --
	| t == 0 = []
	| x == 0 && y == 0 = cornerDownLeft x y t
	| x == 0 && y == (size-1) = cornerUpLeft x y t
	| x == (size-1) && y == 0 = cornerDownRight x y t
	| x == (size-1) && y == (size-1) = cornerUpRight x y t
	-- Seiten:
	| x == 0 = sideLeft x y t
	| y == 0 = sideDown x y t
	| x == (size-1) = sideRight x y t
	| y == (size-1) = sideUp x y t
	-- Normalfall:
	| otherwise = normalMovement x y t

