module GeneralLogic where

{-- ================================================================ --}
{-- Data, Global Variables, Constants ============================== --}
{-- ================================================================ --}

g_steps = 4

size = 4

wumpus = (0,2)

pits = [(2,0), (2,2), (3,2)]

gold = (2,2)

-- Prüft ab, ob Koordinaten das Spielfeld verlassen.
rangeError x y = x < 0 || y < 0 || x >= size || y >= size

-- Negation von Atomen.
getNeg neg
	| neg = ""
	| otherwise = "-"

-- Bastelt den Atomnamen zusammen aus der ID und den Koordinaten. 
-- Unter Umständen negiert.
calcAtomAt id x y neg 
	| rangeError x y = []
	| otherwise = getNeg neg ++ id ++ "_" ++ show x ++ "_" ++ show y

-- Gleich wie calcAtomAt, nur mit einem Zeitstempel angehängt.
calcTimeAtomAt id x y t neg
	| rangeError x y = []
	| otherwise = getNeg neg ++ id ++ "_" ++ show x ++ "_" ++ show y ++ "_" ++ show t

-- Richtung zu einem Zeitschritt.
calcDirAtomAt id t neg = getNeg neg ++ id ++ "_" ++ show t




