--- module (NICHT AENDERN!)
module DeathStacksBot where
    --- imports (NICHT AENDERN!)
    import Data.Char
    import Util
    --speichert die positionen als tubel
    nummber ::(Eq int, Num int) => [(int,int)]
    nummber =  [(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(0,0),(0,1),(0,2),(0,3),(0,4),(0,5)]
    --enfernt doublikate Qulle: stackoverflow
    rmdups :: Eq a => [a] -> [a]
    rmdups [] = []
    rmdups (x:xs)   | x `elem` xs   = rmdups xs
                    | otherwise     = x : rmdups xs

    fd ::(Num a, Ord a, Eq a ) =>  [(a,((a,a),(a,a)))] -> [(a,((a,a),(a,a)))]
    fd []                   = []
    fd ((c,((v,b),(n,m))):xs) = if (v==n && b == m) then fd xs else (c,((v,b),(n,m))) : fd xs 

    
    
    tooTallFilterpre:: (Num a, Ord a, Eq a, Show a) => [(a,(a,a))] -> [(a,(a,a))]
    tooTallFilterpre xs = filter(\x-> (fst x)>4) xs
    
    count ::  [(a,((a,a),(a,a)))] -> a
    count a = fst(head(a)) 

    tooTallFilterpost::(Num a, Ord a, Eq a ) =>   [(a,((a,a),(a,a)))]-> [(a,((a,a),(a,a)))]
    tooTallFilterpost  xs  = filter(\x->((fst x) >= (count(xs)-4))) xs

    calculatedTrains :: (Num a, Ord a, Eq a, Show a) => [(a,(a,a))] -> [(a,((a,a),(a,a)))]
    calculatedTrains [] = []
    calculatedTrains (x:xs) = fd ((nord x) ++ (nordost x) ++ (ost x) ++ (südost x) ++ (süd x) ++ (südwest x) ++ (west x) ++ (nordwest x) ++ calculatedTrains xs )
   



    calculatedfinal :: (Num a, Ord a, Eq a, Show a) => [(a,(a,a))]-> [(a,((a,a),(a,a)))]
    calculatedfinal x = if (tooTallFilterpre x) == [] then calculatedTrains x else tooTallFilterpost( calculatedTrains( tooTallFilterpre x))
    -- +1 da das spielfet  von 1 bis 6 geht die in terne bereh 
    formatIn::(Num a, Ord a, Eq a, Show a) => [(a,((a,a),(a,a)))]->[Char]
    formatIn ((count,((rowO,colO),(rowN,colN))):[]) = (itoa(colO+1)++show(rowO+1)++"-"++show(count)++"-"++itoa(colN+1)++show(rowN+1))
    formatIn ((count,((rowO,colO),(rowN,colN))):xs) = (itoa(colO+1)++show(rowO+1)++"-"++show(count)++"-"++itoa(colN+1)++show(rowN+1))++","++ (formatIn( xs))
    
    formatOut:: [Char] -> [Char]
    formatOut a = "["++a++"]"
    
    format:: (Num a, Ord a, Eq a, Show a) => [(a,((a,a),(a,a)))]->[Char]
    format a =  formatOut(formatIn(rmdups (a))) 

    itoa::(Eq int, Num int) => int -> [Char]
    itoa 1 = "a"
    itoa 2 = "b"
    itoa 3 = "c"
    itoa 4 = "d"
    itoa 5 = "e"
    itoa 6 = "f"
     
    replace :: Eq a => a -> a -> [a] -> [a]
    replace a b = map $ \c -> if c == a then b else c
    
    adjustmentPositon :: (Num a, Ord a) => a -> a
    adjustmentPositon p   =  if (abs(p) < 6) then abs(p) else adjustmentPositon (abs(10-p))
    
    nord ::(Num a, Ord a) => (a,(a,a))  -> [(a,((a,a),(a,a)))]
    nord (0,(x,y)) = []
    nord (n,(x,y)) = (n,((x,y),(adjustmentPositon(x-n),y))): nord ((n-1),(x,y))
    
    süd ::(Num a, Ord a) => (a,(a,a))  -> [(a,((a,a),(a,a)))]
    süd (0,(x,y)) = []
    süd (n,(x,y)) = (n,((x,y),(adjustmentPositon(x+n),y))): süd ((n-1),(x,y))
    
    west ::(Num a, Ord a) => (a,(a,a))  -> [(a,((a,a),(a,a)))]
    west (0,(x,y)) = []
    west (n,(x,y)) = (n,((x,y),(x,adjustmentPositon(y-n)))): west ((n-1),(x,y))
    
    ost ::(Num a, Ord a) => (a,(a,a))  -> [(a,((a,a),(a,a)))]
    ost (0,(x,y)) = []
    ost (n,(x,y)) = (n,((x,y),(x,adjustmentPositon(y+n)))): ost ((n-1),(x,y))
    
    nordost ::(Num a, Ord a) => (a,(a,a))  -> [(a,((a,a),(a,a)))]
    nordost (0,(x,y)) = []
    nordost (n,(x,y)) = (n,((x,y),(adjustmentPositon(x-n),adjustmentPositon(y+n)))): nordost ((n-1),(x,y))
    
    südost ::(Num a, Ord a) => (a,(a,a))  -> [(a,((a,a),(a,a)))]
    südost (0,(x,y)) = []
    südost (n,(x,y)) = (n,((x,y),(adjustmentPositon(x+n),adjustmentPositon(y+n)))): südost ((n-1),(x,y))
    
    südwest ::(Num a, Ord a) => (a,(a,a))  -> [(a,((a,a),(a,a)))]
    südwest (0,(x,y)) = []
    südwest (n,(x,y)) = (n,((x,y),(adjustmentPositon(x+n),adjustmentPositon(y-n)))): südwest ((n-1),(x,y))
    
    nordwest ::(Num a, Ord a) => (a,(a,a))  -> [(a,((a,a),(a,a)))]
    nordwest (0,(x,y)) = []
    nordwest (n,(x,y)) = (n,((x,y),(adjustmentPositon(x-n),adjustmentPositon(y-n)))): nordwest ((n-1),(x,y))
    
    myzip :: [a] -> [(b,c)] -> [(a, c)]
    myzip [] _ = []
    myzip _[]  = [] 
    myzip  (x:xs)  (y:ys) = (x, (snd y)): myzip xs ys 
    -- test imput "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r"
    
    -- ###################################################################################################################################
    --entferne []
    cut :: String->String
    cut a = tail(init(a))
    --teilit string an den kommerta und erstellt liste aus strings 
    split :: String->[String]
    split a = splitOn "," (cut a)
    --wählt erstes element aus liste 
    choose :: String->String
    choose a = head (split (a) )
    -- ###################################################################################################################################
    --- external signatures (NICHT AENDERN!)
    getMove :: String -> String
    getMove a = choose(listMoves a) 
    
    listMoves :: String -> String
    listMoves oneString = 
        --speichert farbe es spieler der drann ist 
        let playerColor = last oneString in 
        ----trennt input in Spielbrett der form "bb,bb,,,,/...."
        let board = init oneString  in
        -- ersetzt Reihentrenner (/) druch Feldtrenner (,) damit im nächsten schritt aus dem gesammt Spielfeldstring eine liste aus Einzelfeldestrings gemacht werden kann  
        let string =  replace '/' ',' board in
        -- Macht aus einem Spielfeldstring eine liste aus Einzelstrings
        let splittString = splitOn "," string in
        -- weisst jedem Stack(spielfigur), seine position als Tupel der form (stack,positon) zu      
        let tupel = zip  splittString nummber  in
        -- filter die Stack die wir bewegen dürfen  
        let ourStack = filter(\x-> isPrefixOf  [playerColor] (fst x) ) tupel in
        -- ermittel wie große jeder stack ist den wir bewegen dürfen 
        let counterList = map (\x-> length (fst x)) ourStack in
        -- erstellt neues Tupel der form (anzahl steine,Positon)
        let newTupel =  myzip counterList ourStack in
       --berechnet alle gültigen züge
        let final = calculatedfinal  newTupel  in 
            --let n = myfst(myhead(newTupel)) in        --berechnet alle möglichen züge
            format( final ) 
            --format(calculatedTrains(newTupel))
             
    
    
    
    
    
