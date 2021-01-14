module Task2 where
import Data.Word ( Word8 )

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read,Eq)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show,Read,Eq)


example :: Image
example = Image 3 2 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0],
                     [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]

grayExample :: Image
grayExample = Image 3 2 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227],
                         [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]


example2 :: Image
example2 = Image 4 5 [[Rgb 71 71 71,    Rgb 41 41 41, Rgb 16 16 16,    Rgb 138 138 138], 
                      [Rgb 26 26 26,    Rgb 40 40 40, Rgb 138 138 138, Rgb 73 73 73] , 
                      [Rgb 184 184 184, Rgb 40 40 40, Rgb 40 40 40,    Rgb 40 40 40] , 
                      [Rgb 40 40 40,    Rgb 40 40 40, Rgb 182 182 182, Rgb 16 16 16] , 
                      [Rgb 39 39 39,    Rgb 81 81 81, Rgb 158 158 158, Rgb 15 15 15]]

-- floodFill pixel 2 1 example2
--                       [Rgb 71 71 71,    Rgb 41 41 41, Rgb 16 16 16,    Rgb 138 138 138],
--                       [Rgb 26 26 26,    Rgb 50 50 50, Rgb 138 138 138, Rgb 73 73 73],
--                       [Rgb 184 184 184, Rgb 50 50 50, Rgb 50 50 50,    Rgb 50 50 50],
--                       [Rgb 50 50 50,    Rgb 50 50 50, Rgb 182 182 182, Rgb 16 16 16],
--                       [Rgb 39 39 39,    Rgb 81 81 81, Rgb 158 158 158, Rgb 15 15 15]]

--    [[Rgb 76 76 76, Rgb 122 122 122,   Rgb 226 226 226],
--    [Rgb 150 150 150, Rgb 255 255 255, Rgb 202 202 202]]

matrixExample = [[1, 2, 3, 4],
                 [5, 6, 7, 8],
                 [9, 10, 11, 12],
                 [13, 14, 15, 16]]


toInt :: (RealFrac a, Integral b) => a -> b
toInt num = round num 

enumGrayPixel :: Rgb -> Rgb
enumGrayPixel pixel = Rgb { red = x, green = x, blue = x }
                      where x = toEnum (toInt ((0.30 * fromInteger (toInteger (red pixel))) +  (0.59 * fromInteger (toInteger (green pixel))) + (0.11 * fromInteger (toInteger (blue pixel)))))

 
grayscale :: Image -> Image
grayscale img = Image { width = width img,
                        height = height img,
                        content = newContent }
                where
                newContent = map (map enumGrayPixel) 
                            (content img)

-- submatrix :: Int -> Int -> [[a]] -> [[a]]
-- submatrix row col matrix = takeCols 3 (dropCols (col - 1) rows)
--     where rows = take 3 (drop (row - 1) matrix)

-- getElement :: Int -> Int -> [[a]] -> a
-- getElement row col matrix = matrix!!row!!col


-- firstRow :: [[Rgb]] -> [Rgb]
-- firstRow = head 

-- removeFirstRow :: [[a]] -> [[a]]
-- removeFirstRow = tail 

-- firstCol :: [[b]] -> [b]
-- firstCol = map head 

-- removeFirstCol :: [[a]] -> [[a]]
-- removeFirstCol = map tail

widthMatrix :: [[a]] -> Int
widthMatrix matrix = length (head matrix)

matrix1 :: [[Integer]]
matrix1 = [[1,0,-1],[2,0,-2], [1,0,-1]]

matrix2 :: [[Integer]]
matrix2 = [[1,2,1],[0,0,0],[-1,-2,-1]]

multiplicate :: [[Integer]] -> [[Integer ]] -> Integer   
multiplicate m1 m2 = (m1!!2!!2 * m2!!0!!0) + (m1!!2!!1 * m2!!0!!1) + (m1!!2!!0 * m2!!0!!2) + (m1!!1!!2 * m2!!1!!0) + (m1!!1!!1 * m2!!1!!1) + (m1!!1!!0 * m2!!1!!2) + (m1!!0!!2 * m2!!2!!0) + (m1!!0!!1 * m2!!2!!1) + (m1!!0!!0 * m2!!2!!2)

enum :: Integer -> Integer -> Integer  -> Integer  -> Integer  -> Integer  -> Integer  -> Integer  -> Integer  -> Rgb
enum a00 a01 a02 a10 a11 a12 a20 a21 a22  =
    let 
        matrix = [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]]
        num1 = multiplicate matrix matrix1
        num2 = multiplicate matrix matrix2
        newValue = toInt (sqrt (fromIntegral (num1^2 + num2^2)))
    in case () of 
    _ | newValue >= 255 -> Rgb { red= toEnum 255, green = toEnum 255, blue = toEnum 255}
      | newValue <= 0   -> Rgb { red= toEnum 0, green = toEnum 0, blue = toEnum 0}
      | otherwise       -> Rgb { red= toEnum newValue, green = toEnum newValue, blue = toEnum newValue}
     
                                        
enumPixel :: Int -> Int -> [[Rgb]] -> Rgb
enumPixel row col m 
    | row == 0 && col == 0                               = enum pixel3 pixel2 pixel3 pixel6 pixel5 pixel6 pixel3 pixel2 pixel3   
    | row == 0 && col == (widthMatrix m -1)              = enum pixel1 pixel2 pixel1 pixel4 pixel5 pixel4 pixel1 pixel2 pixel1
    | row == (length m -1) && col == 0                   = enum pixel9 pixel8 pixel9 pixel6 pixel5 pixel6 pixel9 pixel8 pixel9
    | row == (length m -1) && col == (widthMatrix m -1)  = enum pixel7 pixel8 pixel7 pixel4 pixel5 pixel4 pixel7 pixel8 pixel7
    | row == (length m -1) && col /= 0                   = enum pixel7 pixel8 pixel9 pixel4 pixel5 pixel6 pixel7 pixel8 pixel9
    | row /=0 && col == (widthMatrix m -1)               = enum pixel7 pixel8 pixel7 pixel4 pixel5 pixel4 pixel1 pixel2 pixel1
    | row /= 0 && col == 0                               = enum pixel9 pixel8 pixel9 pixel6 pixel5 pixel6 pixel3 pixel2 pixel3
    | row == 0 && col /= 0                               = enum pixel1 pixel2 pixel3 pixel4 pixel5 pixel6 pixel1 pixel2 pixel3
    | otherwise                                          = enum pixel7 pixel8 pixel9 pixel4 pixel5 pixel6 pixel1 pixel2 pixel3
        where 
            pixel1 = toInteger (red (m!!(row+1)!!(col-1)))
            pixel2 = toInteger (red (m!!(row+1)!!col))
            pixel3 = toInteger (red (m!!(row+1)!!(col+1)))
            pixel4 = toInteger (red (m!!row!!(col-1)))
            pixel5 = toInteger (red (m!!row!!col))
            pixel6 = toInteger (red (m!!row!!(col+1)))
            pixel7 = toInteger (red (m!!(row-1)!!(col-1)))
            pixel8 = toInteger (red (m!!(row-1)!!col))
            pixel9 = toInteger (red (m!!(row-1)!!(col+1)))

makeRow :: Int -> Int -> [[Rgb]] -> [Rgb]
makeRow row col matrix   
    | col == length (head matrix) = []
    | otherwise                   = enumPixel row col matrix : makeRow row (col+1) matrix
    

makeMatrix :: Int -> [[Rgb]] -> [[Rgb]]
makeMatrix row matrix 
    | row == length matrix = []
    | otherwise            = makeRow row 0 matrix : makeMatrix (row + 1) matrix
    

edgeDetect :: Image -> Image
edgeDetect img = 
    let 
        newContent =  makeMatrix 0 (content img) 
    in  Image { 
                width = width img,
                height = height img,
                content = newContent    
               }

-------------------------------------------------------------------------------------------
-- dropCols :: Int -> [[a]] -> [[a]]
-- dropCols n matrix = map (\list -> drop n list) matrix

-- takeCols :: Int -> [[a]] -> [[a]]
-- takeCols n matrix = map (\list -> take n list) matrix


addPixel :: [Rgb] -> Int -> Rgb -> [Rgb]
addPixel list index pixel = (take index list) ++ (pixel: drop (index+1) list)


changePixel :: Int -> Int -> [[Rgb]] -> Rgb -> [[Rgb]]
changePixel row col matrix newPixel = take row matrix ++ (addPixel (head (drop row matrix)) col newPixel :  (tail (drop row matrix)))


checkNeibs :: [[Rgb]] -> Int -> Int -> Rgb -> [(Int, Int)]
checkNeibs matrix row col color 
    | row == -1 || col == -1 || row == length matrix || col == widthMatrix matrix || matrix!!row!!col /= color = []
    | otherwise = [(row, col)]


floodFillHelper :: Rgb -> [[Rgb]] -> [(Int,Int)] -> [[Rgb]]
floodFillHelper _     matrix []    = matrix     
floodFillHelper color matrix queue = let 
    row = fst (head queue)
    col = snd (head queue)
    targetColor = matrix!!row!!col
    newMatrix = changePixel row col matrix color
    newPixels = checkNeibs matrix (row-1) col targetColor ++ checkNeibs matrix (row + 1) col targetColor ++ checkNeibs matrix row (col-1) targetColor ++ checkNeibs matrix row (col+1) targetColor 
    in floodFillHelper color newMatrix (tail queue ++ newPixels)



floodFill :: Rgb -> Int -> Int -> Image -> Image
floodFill color x y img = Image {
                                width = width img,
                                height = height img,
                                content = floodFillHelper color (content img) [(x,y)]}

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


