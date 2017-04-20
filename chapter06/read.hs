
main = do
        putStrLn "Please enter a Double:"
        inpStr <- getLine
        let inpDouble = (read inpStr)::Double
        putStrLn (show inpDouble ++ "x 2 == " ++ show (inpDouble * 2))
        
