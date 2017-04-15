{-# LANGUAGE ScopedTypeVariables #-}
module Main where

    import System.IO
    import ReadArgs
    import CombinatoryCalculator
    import ImageGenerator

    main :: IO ()
    main = do
        (command:: String, x:xs :: [String]) <-readArgs
        let f x             = (putStrLn . show) x
            g (x:y:rest)    = (read x, read y);
            g _             = (256, 256)
            in case command of
                "-p"        -> f $ Parse        $ fromString x
                "-d"        -> f $ Decorate     $ fromString x
                "-f"        -> f $ FindRedexes  $ fromString x
                "-a"        -> f $ Analyze      $ fromString x
                "-s"        -> f $ Step         $ fromString x
                "-n"        -> f $ StepN        (read x) $ fromString $ head xs
                "-t"        -> f $ TraceN       (read x) $ fromString $ head xs
                "-o"        -> f $ TraceNPolish (read x) $ fromString $ head xs
                "-r"        -> f $ Reduce       $ fromString x
                "-v"        -> visualize x $ g xs
                "-w"        -> visualize2 x $ g xs
                _           -> f $ "Unknown command \"" ++ command ++ "\"."


