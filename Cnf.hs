module Cnf (Plf(At, Or, And, Not, Cond, Bicond), convertToCnfList, convertToCnfPlf) where

data Plf =
	At String 
	| Or Plf Plf 
	| And Plf Plf 
	| Not Plf 
	| Cond Plf Plf 
	| Bicond Plf Plf 
	deriving (Show, Eq)
	
-- Negation Normal Form: Negationen ausschließlich direkt von Elementen.	
toNNF (Not (Or a b)) = And (toNNF $ Not a) $ toNNF $ Not b
toNNF (Not (And a b)) = Or (toNNF $ Not a) $ toNNF $ Not b
toNNF (Not (Not a)) = toNNF a
toNNF (And a b) = And (toNNF a) $ toNNF b
toNNF (Or a b) = Or (toNNF a) $ toNNF b
toNNF (Not a) = Not $ toNNF a
toNNF (At a) = At a

-- Eleminiert Conditions jeder Form und wandelt sie in Oder/Und-Operationen um.
eleminateConditions (Cond a b) = eleminateConditions $ Or (Not a) b
eleminateConditions (Bicond a b) = eleminateConditions $ And (Cond a b) $ Cond b a
eleminateConditions (Or a b) = Or (eleminateConditions a) $ eleminateConditions b
eleminateConditions (And a b) = And (eleminateConditions a) $ eleminateConditions b
eleminateConditions (Not a) = Not $ eleminateConditions a
eleminateConditions t = t

-- Wandelt einen beliebigen Term in Konjunktive Normalform um.
toCnf'' (Or a (And b c)) = And (toCnf'' $ Or a b) $ toCnf'' $ Or a c
toCnf'' (Or (And b c) a) = And (toCnf'' $ Or a b) $ toCnf'' $ Or a c
toCnf'' (And a b) = And (toCnf'' a) $ toCnf'' b
toCnf'' (Or a b) = Or (toCnf'' a) $ toCnf'' b
toCnf'' (Not a) = Not $ toCnf'' a
toCnf'' (At a) = At a

-- Ruft die Konversion sooft auf, wie eine And-Verknüpfung tief sitzt. 
toCnf' c t
	| c > 0 = toCnf' (c-1) $ toCnf'' t
	| otherwise = toCnf'' t

-- Wrapper.
toCnf t = toCnf' (andDepthCount t) t

-- Hilfsfunktion, die überprüft, wie tief eine logische AND-Verknüpfung maximal im Baum sitzt.
-- Entsprechend oft muss dann toCnf auf den Ausgangsterm angewandt werden, so dass
-- auch der unten sitzende AND-Operator nach oben gezogen werden kann.
andDepthCount' c (Or a b) = max (andDepthCount' (c+1) a) $ andDepthCount' (c+1) b
andDepthCount' c (And a b) = max c $ max (andDepthCount' (c+1) a) $ andDepthCount' (c+1) b
-- Der Rest ist entweder ein At (Ende) oder ein Not At, da in NNF. Auch Ende.
andDepthCount' c t = 0

-- Wrapper.
andDepthCount t = andDepthCount' 0 t

-- Erstellt die Inhalte der Liste mit OR-Verknüpfungen.
calcOr (Or a b) = calcOr a ++ calcOr b
calcOr (At a) = [a]
calcOr (Not a) = negate $ calcOr a
	where 	
		negate [] = []
		negate (('-':t):_) = [t]
		-- funktioniert NUR, weil wir eine NNF haben und eine Negation NUR vor Atomen haben!!!
		negate (h:_) = ['-' : h];

-- Macht aus einer rekursiven Struktur mit Plf eine Liste von Listen in CNF.
plfToList (And a b) = plfToList a ++ plfToList b
plfToList (Or a b) = [calcOr a ++ calcOr b]
plfToList t = [calcOr t]

-- Wrapper für die Schnittstelle nach außen.
convertToCnfList t = plfToList $ toCnf $ toNNF $ eleminateConditions t
convertToCnfPlf  t = toCnf $ toNNF $ eleminateConditions t

{--
	# L_x_y_t <-> ((L_x_y_t-1 & ~North_t-1 & ~South_t-1 &
    #               ~West_t-1 & ~East_t-1) |
    #              (L_x+1_y_t-1 & West_t-1) |
    #              (L_x-1_y_t-1 & East_t-1) |
    #              (L_x_y-1_t-1 & North_t-1) |
	#              (L_x_y+1_t-1 & South_t-1))
--}
-- Klappt (hoffentlich!)
-- --> Im Vergleich zu Beuster sind jeweils North/South (und wahrscheinlich East/West) vertauscht. 
--     Sonst identisch (ersten 4 Zeilen..)
test = convertToCnfList (Bicond (At "L_x_y_t") 
	(Or (And (At "L_x_y_t-1") (And (Not (At "North_t-1")) (And (Not (At "South_t-1")) (And (Not (At "West_t-1")) (Not (At "East_t-1")))))) 
	(Or (And (At "L_x+1_y_t-1") (At "West_t-1")) 
	(Or (And (At "L_x-1_y_t-1") (At "East_t-1")) 
	(Or (And (At "L_x_y-1_t-1") (At "North_t-1")) 
		(And (At "L_x_y+1_t-1") (At "South_t-1")))))))


