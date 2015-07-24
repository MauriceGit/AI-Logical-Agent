module Wumpus where
import Picosat
import Data.List
import System.IO
import Cnf
import Movement

add x y = x + y

removeUpperCase :: [Char] -> [Char]
removeUpperCase s = [ c | c <- s, elem c ['a'..'z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n-1)

len' :: (Num b) => [a] -> b
len' [] = 0
len' (_:xs) = 1 + len' xs


--multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z  

largestDivisible :: (Integral a) => a 
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  

sum' :: (Num a) => [a] -> a  
--sum' xs = foldl (\all x -> all + x) 0 xs
sum' xs = foldl (+) 0 xs

{-- ================================================================ --}
{-- Data, Global Variables, Constants ============================== --}
{-- ================================================================ --}

atoms = [
	"W_0_0", "W_0_1", "W_0_2", "W_0_3", "W_1_0", "W_1_1", "W_1_2", "W_1_3", "W_2_0", "W_2_1", "W_2_2", "W_2_3", "W_3_0", "W_3_1", "W_3_2", "W_3_3", 
	"P_0_0", "P_0_1", "P_0_2", "P_0_3", "P_1_0", "P_1_1", "P_1_2", "P_1_3", "P_2_0", "P_2_1", "P_2_2", "P_2_3", "P_3_0", "P_3_1", "P_3_2", "P_3_3", 
	"G_0_0", "G_0_1", "G_0_2", "G_0_3", "G_1_0", "G_1_1", "G_1_2", "G_1_3", "G_2_0", "G_2_1", "G_2_2", "G_2_3", "G_3_0", "G_3_1", "G_3_2", "G_3_3"
	]

steps = 6

size = 4

wumpus = (0,2)

pits = [(2,0), (2,2), (3,2)]

gold = (2,2)

{-- ================================================================ --}
{-- Konversionsroutinen der Term-Liste ============================= --}
{-- ================================================================ --}

atomsToNum = zip [1..] atoms

-- Macht den Loockup in der Liste mit Atomen, um die Zahl herauszufinden
-- die dem Atom zugeordnet wurde.
-- Auch für ein negiertes Atom.
findNum' :: (Eq b, Num a) => [(a, b)] -> b -> a
findNum' [] _ 	= 0
findNum' (h:t) a
	| snd h == a = fst h
	| otherwise = findNum' t a

-- Findet die Zahl, die einem Atom zugeordnet wurde!
-- Auch, wenn das Atom negiert wurde!
findNum :: String -> Int
findNum ('-' : a)
	| not $ elem a atoms = 0
	| otherwise = -(findNum' atomsToNum a)
findNum a 
	| not $ elem a atoms = 0
	| otherwise = findNum' atomsToNum a

-- Findet das Atom eines absoluten Wertes.
findAtom' :: Eq a => [(a, [Char])] -> a -> [Char]
findAtom' [] _ = ""
findAtom' (h:t) n
	| fst h == n = snd h
	| otherwise = findAtom' t n

-- Findet das Atom, welches der übergebenen Zahl zugeordnet ist
-- Auch, wenn es negiert wurde!
findAtom :: Int -> String
findAtom n 
	| n < 0 = "-" ++ findAtom' atomsToNum (abs $ n)
	| otherwise = findAtom' atomsToNum n

-- [["Atom1"], ["Atom1", "Atom2"], ["Atom3", "Atom1"]]

-- Hilfsfunktion --> Ersetzt die einzelnen Atome
listAtomToNum' :: [String] -> [Int]
listAtomToNum' [] = []
listAtomToNum' (h:t) = findNum h : listAtomToNum' t

-- Macht aus einer Liste mit ausgeschriebenen Atomen eine Liste mit
-- Integers für den picoSAT-Solver.
listAtomToNum :: [[String]] -> [[Int]]
listAtomToNum [] = []
listAtomToNum (h:t) = [listAtomToNum' h] ++ listAtomToNum t

-- Hilfsfunktion --> Ersetzt die einzelnen Integer
listNumToAtom' :: [Int] -> [String]
listNumToAtom' [] = []
listNumToAtom' (h:t) = findAtom h : listNumToAtom' t

-- Macht aus einer Liste mit Integers aus dem picoSAT eine
-- Liste mit lesbaren Atomen. Listen sind identisch.
listNumToAtom :: [[Int]] -> [[String]]
listNumToAtom [] = []
listNumToAtom (h:t) = [listNumToAtom' h] ++ listNumToAtom t

-- Macht aus der Resultatliste  mit Zahlen eine, die eine lesbare
-- Belegung aller Atome enthält.
listResultToAtoms :: [Int] -> [String]
listResultToAtoms [] = []
listResultToAtoms (h:t) = [findAtom h] ++ listResultToAtoms t
	
-- Bekommt eine Solution aus dem PicoSAT rein und gibt die Liste
-- mit Int zurück --> Belegung der Atome!
solutionToList :: Solution -> [Int]
solutionToList (Solution s) = s
solutionToList s = []
	
-- Überführt die Liste mit Atomen in einen String.
listToString :: [String] -> String
listToString [] = []
listToString (h:[]) = h
listToString (h:t) = h ++ ", " ++ listToString t

{-- ================================================================ --}
{-- Berechnungen der Term-Liste ==================================== --}
{-- ================================================================ --}

-- Prüft ab, ob Koordinaten das Spielfeld verlassen.
rangeError x y = x < 0 || y < 0 || x >= size || y >= size

-- Filtert sowohl in den OR, als auch in den AND-Verknüpfungen alle
-- leeren Elemente raus!
filterOr [] = []
filterOr (h:t) = [[e | e <- h, not $ null e ]] ++ filterOr t
filterAnd xs = [e | e <- xs, not $ null e]
filter' xs = filterAnd $ filterOr xs

-- Negation von Atomen.
getNeg neg
	| neg = ""
	| otherwise = "-"

-- Bastelt den Atomnamen zusammen aus der ID und den Koordinaten. 
-- Unter Umständen negiert.
calcAtomAt id x y neg 
	| rangeError x y = []
	| otherwise = getNeg neg ++ id ++ "_" ++ show x ++ "_" ++ show y

-- Ob es auf dem Feld x y nach Wumpus riecht.
smellAt x y = 	(x-1,y) == wumpus ||
				(x+1,y) == wumpus ||
				(x,y-1) == wumpus ||
				(x,y+1) == wumpus

-- Ob man eine Briese auf dem Feld spürt.
breezeAt x y = 	elem (x-1,y) pits ||
				elem (x+1,y) pits ||
				elem (x,y-1) pits ||
				elem (x,y+1) pits

-- Bastelt einen logischen Wumpus-Ausdruck zusammen für eine Position
-- Einzige Verbindung des Agenten zu Gott-Wissen!
wumpusSense x y
	-- Evtl. den Fall wegoptimieren, dass der Agent nie auf den Wumpus selber läuft...
	-- Mögliche Lösung: | (x,y) == wumpus = [["False"]] ++ [["True"]]
	-- Das ergibt immer false im Gesamtkontext und damit kein Valides Ergebnis mit dem Agenten auf dem Wumpus.
	| (x,y) == wumpus =  filter' $ 	[[calcAtomAt "W" x y True]] ++ 
									[[calcAtomAt "W" (x-1) y False]] ++
									[[calcAtomAt "W" x (y-1) False]] ++
									[[calcAtomAt "W" (x+1) y False]] ++
									[[calcAtomAt "W" x (y+1) False]]
	| smellAt x y = filter' $ 	[[calcAtomAt "W" x y False]] ++
								[	[calcAtomAt "W" (x-1) y True] ++ 
									[calcAtomAt "W" (x+1) y True] ++ 
									[calcAtomAt "W" x (y-1) True] ++ 
									[calcAtomAt "W" x (y+1) True]
								]
	| otherwise = filter' $ [[calcAtomAt "W" x y False]] ++
							[[calcAtomAt "W" (x+1) y False]] ++
							[[calcAtomAt "W" (x-1) y False]] ++
							[[calcAtomAt "W" x (y+1) False]] ++
							[[calcAtomAt "W" x (y-1) False]]

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

-- Bastelt einen logischen Gold-Ausdruck zusammen für eine Position
goldSense x y
	| (x,y) == gold = [[calcAtomAt "G" x y True]]
	| otherwise = [[calcAtomAt "G" x y False]]

-- Zusammenstellung aller Wahrnehmungen, die der Agent haben kann.
-- Im Moment noch sauber getrennt von Gott-Wissen.
calcTermAt x y t = 
	wumpusSense x y ++
	pitSense x y ++
	goldSense x y
	--movement x y t

-- Berechnet den gesamten logischen Term!
-- Nicht optimiert, da sehr viele Felder mehrfach berechnet werden!
-- Z.B. : [["Atom1","Atom2"],["-Atom2"],["Atom3","Atom1"]]
calcTerm x y n
	| rangeError x y || n < 0 = []
	| otherwise = 
		calcTermAt x y (steps - n) ++ 
		calcTerm (x-1) y (n-1) ++
		calcTerm x (y-1) (n-1) ++
		calcTerm (x+1) y (n-1) ++
		calcTerm x (y+1) (n-1)
		
-- Legt alle Eigenschaften fest, die in jedem Fall gelten!
-- z.B. L_0_0_0. --> Startpunkt bei 0/0 im Zeitschritt 0.
calcTermStart x y n = [[(calcAtomAt "L" x y True) ++ "_0"]] ++ calcTerm x y n
	
-- Legt Startpunkt auf 0/0 fest.
calcTermAll n = calcTermStart 0 0 n

--
-- Main. Bildet einen Wrapper um die IO-Komponente des Solvers.
-- Hier darf nicht mehr passieren! Der Rest wird funktional und sauber woanders berechnet!
--
main = do
	solution <- solve $ listAtomToNum $ nub $ calcTermAll steps
	let res = listResultToAtoms $ solutionToList solution
	putStrLn $ listToString res
	return res


