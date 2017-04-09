{-# LANGUAGE ScopedTypeVariables #-}
module Main where

    import System.IO
--    import Codec.Picture
    import ReadArgs
    import CombinatoryCalculator

    main :: IO ()
    main = do
        (command:: String, x:xs :: [String]) <-readArgs
        (putStrLn . show) $ case command of
            "-p"    -> Parse        $ fromString x
            "-d"    -> Decorate     $ fromString x
            "-f"    -> FindRedexes  $ fromString x
            "-a"    -> Analyze      $ fromString x
            "-s"    -> Step         $ fromString x
            "-n"    -> StepN        (read x) $ fromString $ head xs
            "-t"    -> StepNTrace   (read x) $ fromString $ head xs
            "-r"    -> Reduce       $ fromString x
