{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STLProcessor
    ( processSTL
    , STLProcessingError(..)
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Control.Monad (replicateM)
import Data.Word (Word32, Word8, Word16)
import Foreign.Storable (peek, poke, Storable)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr, Ptr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V
import Data.List (sortOn)

-- Data types
data STLProcessingError = InvalidSTLFile String | ProcessingError String deriving (Show)

data Vec3 = Vec3 { vx :: Float, vy :: Float, vz :: Float } deriving (Show)

data Triangle = Triangle { normal :: Vec3, v1 :: Vec3, v2 :: Vec3, v3 :: Vec3 } deriving (Show)

data Matrix = Matrix 
    { m11 :: Float, m12 :: Float, m13 :: Float
    , m21 :: Float, m22 :: Float, m23 :: Float
    , m31 :: Float, m32 :: Float, m33 :: Float
    } deriving Show

type Canvas = V.Vector (V.Vector Char)

-- Constants
asciiChars :: String
asciiChars = " .'`^\",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"

-- Matrix operations
rotateY :: Float -> Matrix
rotateY angle = Matrix c 0 s 0 1 0 (-s) 0 c
    where
        c = cos angle
        s = sin angle

rotateX :: Float -> Matrix
rotateX angle = Matrix 1 0 0 0 c (-s) 0 s c
    where
        c = cos angle
        s = sin angle

multiplyMatrixVector :: Matrix -> Vec3 -> Vec3
multiplyMatrixVector m (Vec3 x y z) = Vec3
    (m11 m * x + m12 m * y + m13 m * z)
    (m21 m * x + m22 m * y + m23 m * z)
    (m31 m * x + m32 m * y + m33 m * z)

-- Helper functions
bresenham :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bresenham (x0, y0) (x1, y1) = points
    where
        steep = abs (y1 - y0) > abs (x1 - x0)
        (x0', y0', x1', y1') = if steep then (y0, x0, y1, x1) else (x0, y0, x1, y1)
        (x0'', y0'', x1'', y1'') = if x0' > x1' then (x1', y1', x0', y0') else (x0', y0', x1', y1')
        dx = x1'' - x0''
        dy = abs (y1'' - y0'')
        ystep = if y0'' < y1'' then 1 else -1
        
        generatePoint x y = if steep then (y, x) else (x, y)
        
        points = go x0'' y0'' (dx `div` 2) []
        
        go x y err acc
            | x > x1'' = acc
            | otherwise = 
                let (y', err') = if err < dy
                                then (y + ystep, err + dx - dy)
                                else (y, err - dy)
                in go (x + 1) y' err' (generatePoint x y : acc)

safeIndex :: Int -> String -> Char
safeIndex i str 
    | i < 0 = head str
    | i >= length str = last str
    | otherwise = str !! i

initCanvas :: Int -> Int -> Canvas
initCanvas width height = V.replicate height (V.replicate width ' ')

updateCanvas :: Int -> Int -> Char -> Canvas -> Canvas
updateCanvas x y char canvas =
    let height = V.length canvas
        width = if height > 0 then V.length (canvas V.! 0) else 0
    in if x >= 0 && x < width && y >= 0 && y < height
       then canvas V.// [(y, (canvas V.! y) V.// [(x, char)])]
       else canvas

getAsciiChar :: Float -> Char
getAsciiChar z =
    let normalized = (z + 1.0) / 2.0
        idx = round $ normalized * (fromIntegral $ length asciiChars - 1)
    in safeIndex idx asciiChars

drawLine :: (Int, Int) -> (Int, Int) -> Float -> Canvas -> Canvas
drawLine p1 p2 z canvas =
    let points = bresenham p1 p2
        char = getAsciiChar z
    in foldr (\(x, y) acc -> updateCanvas x y char acc) canvas points

projectPoint :: Int -> Int -> Matrix -> Matrix -> Vec3 -> (Int, Int, Float)
projectPoint width height rotX rotY v =
    let v' = multiplyMatrixVector rotY $ multiplyMatrixVector rotX v
        scale = 0.6
        offsetX = fromIntegral width / 2
        offsetY = fromIntegral height / 2
        x' = round $ (vx v' * scale + 1.0) * offsetX
        y' = round $ (vy v' * scale + 1.0) * offsetY
    in (x', y', vz v')

processTriangle :: Matrix -> Matrix -> Int -> Int -> Triangle -> Canvas -> Canvas
processTriangle rotX rotY width height (Triangle _ v1' v2' v3') canvas =
    let (x1, y1, z1) = projectPoint width height rotX rotY v1'
        (x2, y2, z2) = projectPoint width height rotX rotY v2'
        (x3, y3, z3) = projectPoint width height rotX rotY v3'
        zAvg = (z1 + z2 + z3) / 3
        withLine1 = drawLine (x1, y1) (x2, y2) zAvg canvas
        withLine2 = drawLine (x2, y2) (x3, y3) zAvg withLine1
        withLine3 = drawLine (x3, y3) (x1, y1) zAvg withLine2
    in withLine3

createAsciiArt :: [Triangle] -> String
createAsciiArt triangles =
    let width = 80
        height = 40
        rotX = rotateX (pi / 6)
        rotY = rotateY (pi / 4)
        sortedTriangles = sortOn (\(Triangle _ v1' v2' v3') -> 
            -(vz v1' + vz v2' + vz v3') / 3) triangles
        canvas = initCanvas width height
        finalCanvas = foldr (processTriangle rotX rotY width height) canvas sortedTriangles
    in unlines $ map V.toList $ V.toList finalCanvas

-- Binary parsing
wordToFloat :: Word32 -> Float
wordToFloat w = unsafePerformIO $ alloca $ \(ptr :: Ptr Word32) -> do
    poke ptr w
    peek (castPtr ptr :: Ptr Float)

getFloat32le :: Get Float
getFloat32le = do
    bytes <- getWord32le
    return $ wordToFloat bytes

getVec3 :: Get Vec3
getVec3 = do
    x <- getFloat32le
    y <- getFloat32le
    z <- getFloat32le
    return $ Vec3 x y z

getTriangle :: Get Triangle
getTriangle = do
    n <- getVec3
    v1' <- getVec3
    v2' <- getVec3
    v3' <- getVec3
    _ <- getWord16le
    return $ Triangle n v1' v2' v3'

processSTL :: BL.ByteString -> Either STLProcessingError String
processSTL content = 
    case runGetOrFail parseSTL content of
        Left (_, _, err) -> Left $ InvalidSTLFile err
        Right (_, _, triangles) -> Right $ createAsciiArt triangles
  where
    parseSTL = do
        _ <- replicateM 80 getWord8
        numTriangles <- getWord32le
        replicateM (fromIntegral numTriangles) getTriangle