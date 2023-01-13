module Blackjack ( Hand(..)
                 , cardValue
                 , subtractAces
                 , isAce
                 , handValue) where
    import Cards

    type Hand = [Card]

    -- Return value of an individual card
    cardValue :: Card -> Int
    cardValue (_, value)
        | value == Cards.Two = 2
        | value == Cards.Three = 3
        | value == Cards.Four = 4
        | value == Cards.Five = 5
        | value == Cards.Six = 6
        | value == Cards.Seven = 7
        | value == Cards.Eight = 8
        | value == Cards.Nine = 9
        | value == Cards.Ace = 11
        | otherwise = 10


    subtractAces :: Int -> Int -> Int
    subtractAces handScore aceCount
        | aceCount > 0 && handScore > 21 = do
            subtractAces (handScore-10) (aceCount-1)
        | otherwise = handScore


    isAce :: Card -> Int
    isAce (_, value)
        | value == Cards.Ace = 1
        | otherwise = 0


    handValue :: Hand -> Int -> Int -> Int
    handValue hand value numAces
        | null hand = subtractAces value numAces
        | otherwise = do
            let card = head hand
            let remainingHand = tail hand
            handValue remainingHand (value + cardValue (card)) (numAces + (isAce (card)))

