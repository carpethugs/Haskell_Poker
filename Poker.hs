module Poker where
    
    -- deal cards =  
       

    -- List shifting (simple notation is the notation provide by the prof, standard notation is the one we will be working on in this program)
    shiftToSimpleNotationFunc x = do
        if suit x ==  1 then x+11
        else x-2
    shiftToSimpleNotation list = map shiftToSimpleNotationFunc list
    shiftToStandardNotationFunc x = do
        if suit x == 12 then x-11
        else x+2
    shiftToStandardNotation list = map shiftToStandardNotationFunc list
    
    --card operations (assumes standard notation)
    suit x = div x 13
    value x = mod x 13

-----------------------------------------------------------------------------------------------------------------------------------------------------------
--Daniels Side

    sortByVal hand = do

    
    getHands cards h1 h2 = do
        if null cards then [(reverse)h1,(reverse)h2]
        else if mod (length cards) 2 == 0 then (getHands ((tail) cards) ((head cards) : h1) h2)
        else getHands ((tail) cards) h1 ((head cards) : h2)


    --Tie breaker funcs
    highestRank h1 h2 = highestRankRev ((reverse) h1) ((reverse) h2) ((reverse) h1) ((reverse) h2)

    highestRankRev h1 h2 a b= do
        if null a then highestSuit h1 h2
        else do
            let v1 =(value (head a))
            let v2 = (value (head b)) 
            if v1 == v2  then highestRankRev h1 h2 (tail a) (tail b)
            else if v1 > v2 then reverse h1
            else reverse h2

    highestSuit h1 h2 = [1]

    tieThreePlus h1 h2 = do 
        if (h1 !! 2) > (h2 !! 2) then h1
        else h2
    
    tieTwoPairs h1 h2 = do
        let av1 = value (h1 !! 1)
        let bv1 = value (h2 !! 1)
        let av2 = value (h1 !! 3)
        let bv2 = value (h2 !! 3)
        if av2 > bv2 then h1
        else if av2 < bv2 then h2
        else if av1 > bv1 then h1
        else if av1 < bv1 then h2
        else highestSuit h1 h2
    
    -- tiePair h1 h2 = do 
