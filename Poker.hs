module Poker where
    
    -- data Hand = Hand Int Int Int Int Int
    -- data Card = Card Char Int

    -- getCard(Card Suit Val) = 

    deal cards =  
       

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
    suit x = mod x 13
    value x = div x 13

    

-----------------------------------------------------------------------------------------------------------------------------------------------------------
--Daniels Side

    sortByVal hand = do
    
    getHands cards = do
    
    --Tie breaker funcs
    highestRank h1 h2 = do 
        if (value (last h1)) == (value (last h2)) then highestRank (tail h1) (tail h1)
        else if (length h1)==0 then 0
        else maximum [value (last h1), value (last h2)]
    
    tieThreePlus h1 h2 = do 
        if (h1 !! 2) > (h2 !! 2) then h1
        else h2
    
    tieTwoPairs h1 h2 = do
        let h1v1 = h1 !! 1
        let h2v1 = h2 !! 1
        let h1v2 = h1 !! 3
        let h2v2 = h2 !! 3
        if h1v2>h2v2 then h1
        else if h1v2<h2v2 then h2
        else if h1v1>h2v1 then h1
        else if h1v1<h2v1 then h2
        else highestRank h1 h2
    
    tiePair h1 h2 = do 
