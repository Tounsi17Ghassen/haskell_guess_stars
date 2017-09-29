check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x==c
          then c
          else y | (x,y) <- zip word display])
guessed :: String ->  Char -> (Bool, String)
guessed prev c
  = (c `elem` prev, prev ++ " " ++ [c])

turn :: String -> String -> String -> Int -> IO ()
turn word display g n =
  do if n==0
       then putStrLn "You lose"
       else if word==display
              then putStrLn "You win!"
              else mkguess word display g n
mkguess :: String -> String -> String -> Int -> IO()
mkguess word display g n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (gsd,g')=guessed g (q!!0)
     let (correct, display') = check word display (q!!0)
     let n' = if (correct || gsd) then n else n-1
     turn word display' g' n'
starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] "" n
