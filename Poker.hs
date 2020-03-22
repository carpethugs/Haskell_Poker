module Poker where

    data Hand = Hand Int Int Int Int Int
    data Card = Card Char Int

    getCard(Card Suit Val) = Suit ++ show Val

    getVal card= (card `mod` 13)

    deal cards =  

