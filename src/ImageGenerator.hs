
module ImageGenerator where

    import Codec.Picture
    import CombinatoryCalculator

    -- |Colour is a simple colouring function for Symbols.
    colour      :: Symbol -> PixelRGB8
    colour A    = PixelRGB8 255 255 255
    colour S    = PixelRGB8 255   0   0
    colour K    = PixelRGB8   0 255   0
    colour I    = PixelRGB8   0   0 255

{-
    visualize   :: String -> (Int,Int) -> IO()
--    visualize expression (width, height) = putStrLn $ "'visualize' called with " ++ expression ++ " and " ++ (show (width, height))
    visualize expression (width, height) =
        writePng (expression ++ ".png")
                 (generateImage (generator width height) width height)

    generator   :: Int -> Int -> Int -> Int -> PixelRGB8
    generator   width height x y =
-}
