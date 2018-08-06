{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW


main :: IO ()
main = runContextT defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (defaultWindowConfig "I am a window.")
    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (n * 3)
    writeBuffer vertexBuffer 0 $ circle n

    shader <- compileShader $ do
        primitiveStream <- toPrimitiveStream id
        fragmentStream <- rasterize (const
            (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream
        drawWindowColor (const
            (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer shader win
  where
    n :: Int
    n = 0x20

    slice :: Int -> Int -> [(V4 Float, V3 Float)]
    slice i n =
        [ (V4  0            0           0 1 , V3 0.5 0 0)
        , (V4 (cos a  / 2) (sin a  / 2) 0 1 , V3 (fromIntegral $ i `mod` 2) 0.5 0)
        , (V4 (cos a' / 2) (sin a' / 2) 0 1 , V3 (fromIntegral $ i `mod` 2) 0.5 0)]
      where n', i' :: Float
            n' = fromIntegral n
            i' = fromIntegral i
            a  = pi * 2 / n' *  i'
            a' = pi * 2 / n' * (i' + 1)

    circle :: Int -> [(V4 Float, V3 Float)]
    circle n = concat $ [ slice i n | i <- [0..n - 1] ]

loop :: (ContextColorFormat c, Num a1, Floating a1, Color c Float ~ V3 a1) => Buffer os a2 -> (PrimitiveArray Triangles a2 -> Render os ()) -> Window os c ds -> ContextT Handle os IO ()
loop vertexBuffer shader win = do
    render $ do
        clearWindowColor win (V3 (0x59 / 0x8ff) (0x35 / 0x8ff) (0xF / 0x8ff))
        vertexArray <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
    swapWindowBuffers win

    closeRequested <- windowShouldClose win
    unless (closeRequested == Just True) $
        loop vertexBuffer shader win

