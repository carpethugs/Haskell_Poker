module Poker where
    
    data Hand = Hand Int Int Int Int Int
    data Card = Card Char Int

    getCard(Card Suit Val) = 


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
    