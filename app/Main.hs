module Main where
    import Cards
    import Blackjack
    import System.IO

    playerHand :: Hand
    playerHand = take 2 (deck)


    dealerHand :: Hand
    dealerHand = take 2 (reverse deck)


    dealerDraw hand
        | handValue hand 0 0 < 17 = do 
            let drawCard = head deck
            putStrLn $ "[*] Dealer draws " ++ (show drawCard)
            dealerDraw (drawCard : hand)
        | otherwise = putStrLn "[*] Done drawing"


    determineWinner :: Int -> Int -> IO ()
    determineWinner playerScore dealerScore
        | playerScore < 100 && dealerScore == 100 = putStrLn("Player")
        | playerScore > dealerScore = putStrLn("Player")
        | playerScore == dealerScore = putStrLn("Tie")
        | playerScore < dealerScore = putStrLn("Dealer")


    prompt :: String -> IO Bool
    prompt message = do
        putStr $ message ++ ": "
        hFlush stdout
        input <- getLine
        case input of
            "y" -> return True
            "n" -> return False
            _   -> do
                putStrLn "Invalid input"
                prompt message


    playRound :: Hand -> IO ()
    playRound hand = do
        putStrLn "[*] Player Hand:"
        putStrLn $ "  - Value = " ++ show (handValue hand 0 0)
        putStrLn $ "  - Cards = " ++ show hand 

        playerHits <- prompt "[?] Hit or stay? (Hit = y, Stay = n)"
        if playerHits
            then do
                let drawCard = head deck
                let newHand = drawCard : hand
                putStrLn $ "[*] Player draws " ++ show drawCard

                -- Return if drawing a new card results in a bust
                -- Otherwise, prompt player again
                if handValue newHand 0 0 > 21
                    then do
                        putStrLn "[!] Winner: Dealer"
                        return ()
                    else playRound newHand
            else do
                -- Perform dealer's turn after player stands
                putStrLn "[*] Dealer Drawing"
                dealerDraw dealerHand

                -- Determine the round's winner
                putStr "[!] Winner: "
                determineWinner (handValue playerHand 0 0) (handValue dealerHand 0 0)

                return ()


    banner :: IO ()
    banner = do
        putStrLn "\
         \ _____\n\
         \|A .  | _____\n\
         \| /.\\ ||A ^  | _____\n\
         \|(_._)|| / \\ ||A _  | _____\n\
         \|  |  || \\ / || ( ) ||A_ _ |\n\
         \|____V||  .  ||(_'_)||( v )|\n\
         \       |____V||  |  || \\ / |\n\
         \              |____V||  .  |\n\
         \                     |____V|\n"
        putStrLn "[#] darkedge v0.0.1"
        putStrLn "~ Always account for variable change ~\n"


    main :: IO ()
    main = do
        banner

        playRound playerHand

        continue <- prompt "[?] Play again? (y/n)"
        if continue then main else return ()

