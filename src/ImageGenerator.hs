
module ImageGenerator where

    import Codec.Picture
    import Debug.Trace
    import CombinatoryCalculator

    -- |Colour is a simple colouring function for Symbols.
    colour      :: Symbol -> PixelRGB8
    colour A    = PixelRGB8 255 255 255
    colour S    = PixelRGB8 255   0   0
    colour K    = PixelRGB8   0 255   0
    colour I    = PixelRGB8   0   0 255

    black       :: PixelRGB8
    black       = PixelRGB8   0   0   0

    visualize   :: String -> (Int,Int) -> IO()
    visualize expression (width, height) =
        let lines   = case (tryStepNList height) (fromString expression) of
                      Just xs -> map fromParseTree xs;
                      Nothing -> take height $ repeat []
            lengths = map length lines
        in  writePng (expression ++ ".png")
            (generateImage (generator lines lengths) width height)

    generator   :: [Expression] -> [Int] -> Int -> Int -> PixelRGB8
    generator lines lengths x y =
        if (lengths !! y > x) then colour ((lines !! y) !! x) else black
