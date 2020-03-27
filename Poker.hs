module Poker where
    import Data.List
    
    deal cards =  do
        let hands = getHands cards
        let h1 = shiftToSimpleNotation(head hands)
        let h2 = shiftToSimpleNotation(last hands)
        let v1 = determineHandType h1
        let v2 = determineHandType h2
        if v1 < v2 then formatOut h1
        else if v1 > v2 then formatOut h2
        else formatOut(tieBreaker h1 h2 v1)

    formatOut hand = sort(handToString(shiftToStandardNotation hand))
    --work on this later ->>> deal cards =  
    
    runTestCases = do
        let testCases = [[17, 10, 15, 7, 14, 8, 16, 11, 18, 9],[18, 8, 14, 11, 17, 10, 15, 9, 16, 7],[15, 22, 18, 49, 17, 48, 16, 9, 14, 35],[14, 35, 18, 49, 16, 22, 17, 48, 15, 9],[17, 47, 3, 48, 32, 46, 31, 10, 2, 24],[31, 46, 17, 48, 2, 47, 32, 24, 3, 10],[13, 49, 8, 28, 41, 51, 7, 29, 6, 47],[8, 49, 13, 47, 7, 28, 6, 29, 41, 51],[16, 38, 15, 52, 18, 51, 14, 39, 17, 26],[17, 51, 14, 52, 16, 39, 15, 26, 18, 38],[17, 9, 18, 8, 16, 10, 14, 11, 15, 7],[16, 10, 15, 9, 17, 11, 18, 8, 14, 7],[18, 48, 15, 22, 17, 49, 14, 9, 16, 35],[14, 49, 18, 22, 16, 35, 17, 9, 15, 48],[3, 39, 2, 13, 30, 12, 32, 26, 31, 11],[2, 11, 30, 39, 31, 13, 32, 26, 3, 12],[33, 49, 46, 51, 47, 52, 20, 40, 7, 50],[47, 52, 33, 40, 20, 50, 7, 49, 46, 51],[17, 26, 15, 38, 18, 39, 16, 51, 14, 52],[18, 38, 16, 52, 15, 26, 14, 39, 17, 51],[32, 26, 2, 12, 30, 4, 31, 13, 3, 25],[2, 4, 3, 13, 32, 26, 30, 25, 31, 12],[32, 4, 2, 24, 30, 13, 3, 12, 31, 26],[3, 26, 32, 24, 2, 13, 30, 12, 31, 4],[3, 13, 32, 12, 31, 22, 30, 4, 2, 24],[3, 13, 32, 12, 31, 4, 2, 22, 30, 24],[34, 40, 26, 52, 13, 50, 21, 51, 8, 49],[26, 52, 21, 49, 8, 51, 13, 50, 34, 40]]
        let answerKey = [["10C", "11C", "7C", "8C", "9C"],["10C", "11C", "7C", "8C", "9C"],["1D", "2D", "3D", "4D", "5D"],["1D", "2D", "3D", "4D", "5D"],["10C", "11D", "7S", "8S", "9S"],["10C", "11D", "7S", "8S", "9S"],["13C", "2S", "6C", "7C", "8C"],["13C", "2S", "6C", "7C", "8C"],["1D", "2D", "3D", "4D", "5D"],["1D", "2D", "3D", "4D", "5D"],["10C", "11C", "7C", "8C", "9C"],["10C", "11C", "7C", "8C", "9C"],["1D", "2D", "3D", "4D", "5D"],["1D", "2D", "3D", "4D", "5D"],["2C", "3C", "4H", "5H", "6H"],["2C", "3C", "4H", "5H", "6H"],["10S", "11S", "12S", "13S", "1S"],["10S", "11S", "12S", "13S", "1S"],["1D", "2D", "3D", "4D", "5D"],["1D", "2D", "3D", "4D", "5D"],["2C", "3C", "4H", "5H", "6H"],["2C", "3C", "4H", "5H", "6H"],["2C", "3C", "4H", "5H", "6H"],["2C", "3C", "4H", "5H", "6H"],["2C", "3C", "4H", "5H", "6H"],["2C", "3C", "4H", "5H", "6H"],["10S", "11S", "12S", "13S", "1S"],["10S", "11S", "12S", "13S", "1S"]]
        checkTestCases testCases answerKey [] []
        
    checkTestCases test answer failed passed
        | null test = [passed, failed]
        | otherwise = do
            let input = head test
            let output =  head answer
            if ( deal input == output ) then checkTestCases ((tail) test) ((tail) answer) failed (passed ++ [((map) show input)])
            else checkTestCases ((tail) test) ((tail) answer) (failed ++ ( ((map) show output) : ((map) show ((deal) input)) : ((map) show input) :[] ) ) passed

    --addTestCasesResultsTogether input output expected = expected : output : ((map) show input) : []

    -- List shifting (simple notation is the notation provide by the prof, standard notation is the one we will be working on in this program)
    shiftToSimpleNotationFunc x = do
        if value x ==  1 then x+11
        else x-2
    shiftToSimpleNotation list = map shiftToSimpleNotationFunc list
    shiftToStandardNotationFunc x = do
        if value x == 12 then x-11
        else x+2
    shiftToStandardNotation list = map shiftToStandardNotationFunc list
    
    --card operations (assumes standard notation)
    suit x = div x 13
    value x = mod x 13

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
        else if ( isStraightFlush hand ) then 1
        else if ( isFourOfAKind hand ) then 2
        else if ( isFullHouse hand ) then 3
        else if ( isFlush hand ) then 4
        else if ( isStraight hand ) then 5
        else if ( isThreeOfAKind hand ) then 6
        else if ( isDoublePair hand ) then 7
        else if ( isSinglePair hand ) then 8
        else 9
    
    isRoyalFlush hand = do
        if ( isStraightFlush hand && isFlush hand && containsValueWithFunc hand 12 value && containsValueWithFunc hand 11 value) then True
        else False

    isStraightFlush hand = do
        if( isStraight hand && isFlush hand ) then True
        else False
    
    isFlushOld hand = do elementRepeatExact hand ((head)hand) suit 5

    isFlush hand = do
        flushHelper ((tail)hand) (suit((head) hand))
    
    flushHelper hand val = do
        if null hand then True
        else if suit(head hand) == val then flushHelper ((tail) hand) (val)
        else False

    isStraight hand = do
        let aceLow = map (\x -> if value x == 12 then x -12 else x+1) hand
        if(isStraightCheck hand || isStraightCheck aceLow ) then True
        else False
    --isStraight sub method
    isStraightCheck hand = do
        let handByValue = sortHandByMethod hand [] sortByValueFirst
        isDescendingFrom ((tail) handByValue) ((head) handByValue)
    --isStraight sub method
    isDescendingFrom hand desFrom
        | null hand = True
        | not (value ((head) hand) == ((value) desFrom) - 1) = False  
        | otherwise = isDescendingFrom ((tail) hand) ((head) hand)
    
    isFourOfAKind hand = anyElementRepeats hand value 4 0 --the 4 because looking for a four of a kind

    isFullHouse hand = anyElementRepeats hand value 3 0 && anyElementRepeats hand value 2 0

    isThreeOfAKind hand = anyElementRepeats hand value 3 0

    isDoublePair hand = isDoublePairHelper hand value 2 0

    isDoublePairHelper hand func number tempIndex
        | tempIndex == length hand = False--5 because that is the size of a hand
        | elementRepeatExact hand (hand !! tempIndex) func number = do
            let filteredHand = removeAllCardsByFaceValue hand (hand !! tempIndex) []
            anyElementRepeats filteredHand value 2 0
        | otherwise = anyElementRepeats hand func number (tempIndex+1)

    isSinglePair hand = anyElementRepeats hand value 2 0
    ---------------Helper functions-------------------

    --hand is the list that you are serching
    --value is the value you are looking for
    --func is the func by which the card is evaluated to(pass in suit or value)
    containsValueWithFunc hand value func 
        | hand == [] = False
        | func ((head) hand) == value = True
        | otherwise = containsValueWithFunc ((tail) hand) value func
    
    --returns true is card1 is larger or equal to card 2(same for sort by suit)
    sortByValueFirst card1 card2 = do
        if value card1 > value card2 then True
        else if value card1 < value card2 then False
        else if suit card1 < suit card2 then False
        else True

    sortBySuitFirst card1 card2 = do
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

    elementRepeatExact hand element func number
        | null hand = (number == 0)
        | otherwise = if func ((head) hand) == func element then elementRepeatExact ((tail)hand) element func (number -1) else  elementRepeatExact ((tail)hand) element func number  

    --number is the repeat amount that you are looking for
    --just set temp index to zero (it is used to keep track of where you currently are)
    anyElementRepeats hand func number tempIndex
        | tempIndex == length hand = False
        | elementRepeatExact hand (hand !! tempIndex) func number = True
        | otherwise = anyElementRepeats hand func number (tempIndex+1)

    --Note that this function will return an array is reversed order
    removeAllCardsByFaceValue hand faceValue returnHand = do
        if( null hand ) then returnHand
        else if(value ((head) hand) == value faceValue) then removeAllCardsByFaceValue ((tail) hand) faceValue returnHand
        else removeAllCardsByFaceValue ((tail) hand) faceValue (((head) hand):returnHand)

-----------------------------------------------------------------------------------------------------------------------------------------------------------
--Daniels Side

    handToString hand = do
        if null hand then []
        else cardToString (head hand) : handToString (tail hand)

    cardToString card = do
       show(value card) ++ (getSuitString card)
        
    getSuitString card = do
        let suits = ["C","D","H","S"]
        if suit card == 4 then suits !! (3)
        else suits !! (suit card)

    sortByVal hand = sortBy (\a b -> compare ((value) a) ((value) b)) hand
    sortBySuit2 hand = sortBy(\a b -> compare ((suit) a) ((suit) b)) hand
    
    getHands cards = getHandsHelp cards [] []

    getHandsHelp cards h1 h2 = do
        if null cards then [(reverse)h1,(reverse)h2]
        else if mod (length cards) 2 == 0 then (getHandsHelp ((tail) cards) ((head cards) : h1) h2)
        else getHandsHelp ((tail) cards) h1 ((head cards) : h2)

    tieBreaker a b tieVal= do
        let h1 = sortByVal a
        let h2 = sortByVal b
        case tieVal of
            0 -> highestRank h1 h2
            1 -> highestRank h1 h2
            2 -> tieThreePlus h1 h2
            3 -> tieThreePlus h1 h2
            4 -> highestRank h1 h2
            5 -> highestRank h1 h2
            6 -> tieThreePlus h1 h2
            7 -> tieTwoPairs h1 h2
            8 -> tiePair h1 h2
            _ -> highestRank h1 h2


    --Tie breaker funcs
    highestRank h1 h2 = do
         let aceLowH1 = map (\x -> if value x == 12 then x -12 else x+1) h1
         let aceLowH2 = map (\x -> if value x == 12 then x -12 else x+1) h2
         if (isStraight h1 && not(isRoyalFlush h1)) then highRankHelper aceLowH1 aceLowH2
         else highRankHelper h1 h2

    highRankHelper h1 h2 = highestRankRev ((reverse) h1) ((reverse) h2) ((reverse) h1) ((reverse) h2)

    highestRankRev h1 h2 a b= do
        if null a then highestSuit h1 h2
        else do
            let v1 =(value (head a))
            let v2 = (value (head b)) 
            if v1 == v2  then highestRankRev h1 h2 (tail a) (tail b)
            else if v1 > v2 then reverse h1
            else reverse h2

    highestSuit h1 h2 = do
        let sh1 = sortBySuit2 h1
        let sh2 = sortBySuit2 h2
        highestSuitRev ((reverse) h1) ((reverse) h2) ((reverse) h1) ((reverse) h2)

    highestSuitRev h1 h2 a b= do
        let v1 =(suit (head a))
        let v2 = (suit (head b)) 
        if v1 == v2  then highestSuitRev h1 h2 (tail a) (tail b)
        else if v1 > v2 then reverse h1
        else reverse h2

    --Works for threepair, four of a kind and full house
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
    
    tiePair h1 h2 = do
        let v1 = getPairVal h1
        let v2 = getPairVal h2
        if v1 > v2 then h1
        else if v1 < v2 then h2
        else highestRank h1 h2
         
    getPairVal hand = getPairValHelp ((sortByVal) hand) (-1)

    getPairValHelp hand currentVal = do
        if null hand then (-1)
        else if value (head hand) == currentVal then currentVal
        else getPairValHelp ((tail) hand) (value(head hand)) 