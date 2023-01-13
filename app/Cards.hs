module Cards ( Deck(..)
             , Card(..)
             , Suit(..)
             , Value(..)
             , deck 
             , suits
             , values) where

type Deck = [Card]

type Card = (Suit, Value)

data Suit = Club | Diamond | Heart | Spade
            deriving (Eq, Enum, Show)

data Value = Two   | Three | Four | Five | Six   | Seven
           | Eight | Nine  | Ten  | Jack | Queen | King  | Ace
             deriving (Eq, Enum, Ord, Show)

suits :: [Suit]
suits = [Club .. Spade]

values :: [Value]
values = [Two .. Ace]

deck :: Deck
deck = [(suit, value) | suit <- suits, value <- values]

