main = do
    putStrLn "Greets, how ya call'd?"
    inpStr <- getLine
    putStrLn ("Welome " ++ inpStr ++ "!")
