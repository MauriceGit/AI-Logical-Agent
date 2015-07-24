import Picosat
import Data.List
import System.IO
import Cnf
import Movement
import GeneralLogic

{-- ================================================================ --}
{-- Konversionsroutinen der Term-Liste ============================= --}
{-- ================================================================ --}

--atomsToNum = zip [1..] atoms

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
findNum ('-' : a) aList = -(findNum' aList a)
findNum a aList = findNum' aList a

-- Findet das Atom eines absoluten Wertes.
findAtom' :: Eq a => [(a, [Char])] -> a -> [Char]
findAtom' [] _ = ""
findAtom' (h:t) n
	| fst h == n = snd h
	| otherwise = findAtom' t n

-- Findet das Atom, welches der übergebenen Zahl zugeordnet ist
-- Auch, wenn es negiert wurde!
findAtom n aList
	| n < 0 = "-" ++ findAtom' aList (abs $ n)
	| otherwise = findAtom' aList n

-- Hilfsfunktion --> Ersetzt die einzelnen Atome
listAtomToNum' [] _ = []
listAtomToNum' (h:t) aList = findNum h aList : listAtomToNum' t aList

-- Macht aus einer Liste mit ausgeschriebenen Atomen eine Liste mit
-- Integers für den picoSAT-Solver.
listAtomToNum [] _ = []
listAtomToNum (h:t) aList = [listAtomToNum' h aList] ++ listAtomToNum t aList

-- Hilfsfunktion --> Ersetzt die einzelnen Integer
listNumToAtom' [] _ = []
listNumToAtom' (h:t) aList = findAtom h aList : listNumToAtom' t aList

-- Macht aus einer Liste mit Integers aus dem picoSAT eine
-- Liste mit lesbaren Atomen. Listen sind identisch.
listNumToAtom [] _ = []
listNumToAtom (h:t) aList = [listNumToAtom' h aList] ++ listNumToAtom t aList

-- Macht aus der Resultatliste  mit Zahlen eine, die eine lesbare
-- Belegung aller Atome enthält.
listResultToAtoms [] _ = []
listResultToAtoms (h:t) aList = [findAtom h aList] ++ listResultToAtoms t aList
	
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

-- Filtert sowohl in den OR, als auch in den AND-Verknüpfungen alle
-- leeren Elemente raus!
filterOr [] = []
filterOr (h:t) = [[e | e <- h, not $ null e ]] ++ filterOr t
filterAnd xs = [e | e <- xs, not $ null e]
filter' xs = filterAnd $ filterOr xs

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
goldSense x y t
	-- symbolisiert das Aufnehmen des Goldes!
	| (x,y) == gold = [[calcDirAtomAt "G" t True]]
	| otherwise = [[calcDirAtomAt "G" t False]]

-- Die eigentliche Wahrnehmung zusammengefasst.
senses x y t = 
	wumpusSense x y ++
	pitSense x y ++
	goldSense x y t ++
	calcMovement x y t

-- Zusammenstellung aller Wahrnehmungen, die der Agent haben kann.
-- Im Moment noch sauber getrennt von Gott-Wissen.
calcTermAt x y t = senses x y t

-- Berechnet den gesamten logischen Term!
-- Nicht optimiert, da sehr viele Felder mehrfach berechnet werden!
-- Z.B. : [["Atom1","Atom2"],["-Atom2"],["Atom3","Atom1"]]
calcTerm x y n
	| rangeError x y || n < 0 = []
	| otherwise = 
		calcTermAt x y (g_steps - n) ++ 
		calcTerm (x-1) y (n-1) ++
		calcTerm x (y-1) (n-1) ++
		calcTerm (x+1) y (n-1) ++
		calcTerm x (y+1) (n-1)
		
-- Legt alle Eigenschaften fest, die in jedem Fall gelten!
-- z.B. L_0_0_0. --> Startpunkt bei 0/0 im Zeitschritt 0.
calcTermStart x y n = [[calcTimeAtomAt "L" x y 0 True]] ++ calcTerm x y n -- ++ [[calcDirAtomAt "G" 0 False]] ++ [[calcDirAtomAt "G" n True]]
	
-- Legt Startpunkt auf 0/0 fest.
calcTermAll n = nub $ calcTermStart 0 0 n

-- Extrahiert aus den OR-Verknüpfungen alle Atome und entfernt Negationen!
calcAtomList' [] = []
calcAtomList' (h:t) = extractElement h : calcAtomList' t
	where 	
		extractElement [] = []
		extractElement ('-':s) = s
		extractElement s = s
		
-- Listet alle Atome!
calcAtomList [] = []
calcAtomList (h:t) = calcAtomList' h ++ calcAtomList t

-- Extrahiert Location und Richtungen für einen Zeitstempel t aus der Liste l.
getLocAtTime [] _ _ = []
getLocAtTime (h:t) time loc
	| last h == time && head h == loc = h : getLocAtTime t time loc
	| otherwise = getLocAtTime t time loc
	
checkDirForTime dir time
	| last dir == time = True
	| otherwise = False
	
-- Checkt String auf eine Richtung.
getDirAtTime' dir time
	| (take 4 dir == "East" || take 4 dir == "West" || take 5 dir == "South" || take 5 dir == "North") = dir
	| otherwise = []
	
getDirAtTime [] _ = []
getDirAtTime (('-':h):t) time = getDirAtTime t time
getDirAtTime (h:t) time = getDirAtTime' h time : getDirAtTime t time

-- Extrahiert alle positiven Atome, weil nur die wichtig für das Ergebnis sind.
extractPositiveAtoms [] = []
extractPositiveAtoms (('-':_):t) = extractPositiveAtoms t
extractPositiveAtoms (h:t) = h : extractPositiveAtoms t

--
-- Main. Bildet einen Wrapper um die IO-Komponente des Solvers.
-- Hier darf nicht mehr passieren! Der Rest wird funktional und sauber woanders berechnet!
--
main = do
	let term = calcTermAll g_steps
	-- $ Mach ne Liste mit Atomen (nub) und zip sie mit Zahlen. Vll in Variable zwischenspeichern zur Rückumwandlung!
	let atoms = zip [1..] $ nub $ calcAtomList term
	solution <- solve $ listAtomToNum term atoms
	let res = listResultToAtoms (solutionToList solution) atoms
	--putStrLn $ listToString res
	putStrLn $ listToString $ getLocAtTime res '0' 'L'
	putStrLn $ listToString $ getDirAtTime res '0'
	putStrLn ""
	putStrLn $ listToString $ extractPositiveAtoms res
	putStrLn ""
	return res

--
-- TEST!
--
--move = [[(calcAtomAt "L" 0 0 True) ++ "_0"]] ++ [[(calcAtomAt "L" 1 0 False) ++ "_0"]] ++ [[(calcAtomAt "L" 0 1 False) ++ "_0"]] ++ [[(calcAtomAt "L" 0 2 False) ++ "_0"]] ++ [[(calcAtomAt "L" 1 1 False) ++ "_0"]] ++ 
--	calcMovement 0 1 1
	
move = [[(calcTimeAtomAt "L" 0 0 0 True)]]
	
zipped = zip [1..] $ calcAtomList move
test = do
	solution <- solve $ listAtomToNum move zipped
	let res = listResultToAtoms (solutionToList solution) zipped
	putStrLn $ listToString $ extractPositiveAtoms res
	--putStrLn $ listToString res
	return 0
	







