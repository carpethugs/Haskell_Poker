module Poker where
    
<<<<<<< HEAD
    -- deal cards =  
=======

    --work on this later ->>> deal cards =  
>>>>>>> e04021f38084a6621b4e9cfbcce2247a4c1f39cf
       

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
<<<<<<< HEAD
    suit x = div x 13
    value x = mod x 13
=======
    value x = mod x 13
    suit x = div x 13

    {-
    Royal flush = 0
    straight flush = 1
    four of a kind = 2
    full house = 3
    flush = 4
    straigh = 5
    three of a kind =6
    two pair = 7
    one pair = 8
    high card = 9
    -}
    determineHandType hand = do
        if ( isRoyalFlush hand ) then 0
        else 9
    
    isRoyalFlush hand = do
        if ( isStraightFlush hand && containsValueWithFunc hand 12 value) then True
        else False

    isStraightFlush hand = do
        if( isStraight hand && isFlush hand ) then True
        else False
    
    isStraight hand = do
        let handByValue = sortHandByMethod hand [] sortByValue
        isDescendingFrom ((tail) handByValue) ((head) handByValue)
    
    isDescendingFrom hand desFrom
        | null hand = True
        | not (value ((head) hand) == ((value) desFrom) - 1) = False  
        | otherwise = isDescendingFrom ((tail) hand) ((head) hand)
    

    ---------------Helper functions-------------------

    --hand is the list that you are serching
    --value is the value you are looking for
    --func is the func by which the card is evaluated to(pass in suit or value)
    containsValueWithFunc hand value func 
        | hand == [] = False
        | func ((head) hand) == value = True
        | otherwise = containsValueWithFunc ((tail) hand) value func
    
    --returns true is card1 is larger or equal to card 2(same for sort by suit)
    sortByValue card1 card2 = do
        if value card1 > value card2 then True
        else if value card1 < value card2 then False
        else if suit card1 < suit card2 then False
        else True

    sortBySuit card1 card2 = do
        if suit card1 > suit card2 then True
        else if suit card1 < suit card2 then False
        else if value card1 < value card2 then False
        else True
    
    sortHandByMethod unsorted sorted func
        | length unsorted == 1 = sorted ++ unsorted
        | ((func) ((head) unsorted) (unsorted !! 1)) == False = do
            let eleToSwapUp = head unsorted
            let eleToSwapDown = unsorted !! 1
            let partiallySorted = (sorted) ++ [eleToSwapDown,eleToSwapUp] ++ (tail ((tail) unsorted))
            sortHandByMethod partiallySorted [] func
        | otherwise = sortHandByMethod (tail unsorted) (sorted ++ [((head) unsorted)]) func
>>>>>>> e04021f38084a6621b4e9cfbcce2247a4c1f39cf

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
