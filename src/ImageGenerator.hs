
module ImageGenerator where

    import Codec.Picture
    import Debug.Trace
    import CombinatoryCalculator

    -- |colour is a simple colouring function for Symbols.
    colour      :: Symbol -> PixelRGB8
    colour A    = PixelRGB8 255 255 255
    colour S    = PixelRGB8 255   0   0
    colour K    = PixelRGB8   0 255   0
    colour I    = PixelRGB8   0   0 255

    -- |colour2 is another colouring scheme for expressions in infix notation.
    colour2     :: Char -> PixelRGB8
    colour2 '(' = PixelRGB8 255   0   0
    colour2 'S' = PixelRGB8 204 255   0
    colour2 'K' = PixelRGB8 0   255 102
    colour2 'I' = PixelRGB8 0   102 255
    colour2 ')' = PixelRGB8 204   0 255

    -- |A constant colour for filling empty space in the image.
    black       :: PixelRGB8
    black       = PixelRGB8   0   0   0

--    gen         :: a -> PixelRGB -> [[a]] -> [Int] -> Int -> PixelRGB
--    gen colour lines lengths x y = if (lengths !! y > x) then colour ((lines !! y) !! x) else black

    -- |Visualizes the reduction process of a CL expression in Polish notation.
    visualize   :: String -> (Int,Int) -> IO()
    visualize expression (width, height) =
        let lines   = case (tryStepNList height) (fromString expression) of
                      Just xs -> map fromParseTree xs;
                      Nothing -> take height $ repeat []
            lengths = map length lines
        in  writePng (expression ++ ".png")
            (generateImage (generator lines lengths) width height)

    -- |Performs the mapping from Symbols to Pixels in Polish notation.
    generator   :: [Expression] -> [Int] -> Int -> Int -> PixelRGB8
    generator lines lengths x y =
        if (lengths !! y > x) then colour ((lines !! y) !! x) else black

    -- |Visualizes the reduction process of a CL expression in infix notation.
    visualize2   :: String -> (Int,Int) -> IO()
    visualize2 expression (width, height) =
        let lines   = case (tryStepNList height) (fromString expression) of
                      Just xs -> map show xs;
                      Nothing -> take height $ repeat []
            lengths = map length lines
        in  writePng (expression ++ ".png")
            (generateImage (generator2 lines lengths) width height)

    -- |Performs the mapping from Symbols to Pixels in infix notation.
    generator2   :: [String] -> [Int] -> Int -> Int -> PixelRGB8
    generator2 lines lengths x y =
        if (lengths !! y > x) then colour2 ((lines !! y) !! x) else black


