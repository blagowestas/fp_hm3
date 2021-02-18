module Task2 where
import Data.Word ( Word8 )
import System.IO ()
import Control.Monad.State (join)

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read)

instance Eq Rgb where Rgb red1 green1 blue1 == Rgb red2 green2 blue2 = red1 == red2 && green1 == green2 && blue1 == blue2

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show,Read)

instance Eq Image where
                     Image width1 height1 content1 == Image width2 height2 content2 = width1 == width2 && height1 == height2 && content1 == content2


example :: Image
example = Image 3 2 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0],
                     [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]

grayExample :: Image
grayExample = Image 3 2 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227],
                         [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]



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

multiply :: [[Integer]] -> [[Integer ]] -> Integer   
multiply m1 m2 = (m1!!2!!2 * m2!!0!!0) + (m1!!2!!1 * m2!!0!!1) + (m1!!2!!0 * m2!!0!!2) + (m1!!1!!2 * m2!!1!!0) + (m1!!1!!1 * m2!!1!!1) + (m1!!1!!0 * m2!!1!!2) + (m1!!0!!2 * m2!!2!!0) + (m1!!0!!1 * m2!!2!!1) + (m1!!0!!0 * m2!!2!!2)

enum :: Integer -> Integer -> Integer  -> Integer  -> Integer  -> Integer  -> Integer  -> Integer  -> Integer  -> Rgb
enum a00 a01 a02 a10 a11 a12 a20 a21 a22  =
    let 
        matrix = [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]]
        num1 = multiply matrix matrix1
        num2 = multiply matrix matrix2
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

getRow :: [Rgb] -> String 
getRow []     = ""
getRow (x:xs) = show (red x) ++ " " ++ show (green x) ++ " " ++ show (blue x) ++ "\n" ++ (getRow xs)

getContent :: [[Rgb]] -> String
getContent []     = ""
getContent (x:xs) = getRow x ++ getContent xs

makeString :: Image -> String
makeString img = "P3\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n255\n" ++ (getContent (content img))

saveImage :: FilePath -> Image -> IO()
saveImage path img = writeFile path (makeString img)

-----------------------------------------------------------------------------------

makePixel :: String -> Rgb
makePixel str = 
    let 
        list = words str
        red   =  read (head list)
        green =  read (list!!1) 
        blue  =  read (list!!2) 
    in 
        Rgb red green blue

getPixels :: [String] -> [Rgb]
getPixels = map makePixel

--можем да проверим дали имаме грешка ако w*x/= length list
separatePixels :: Int -> Int -> [Rgb] -> [[Rgb]]
separatePixels _ _ []     = []
--separatePixels _ 0 pixels = [] 
-- separatePixels _ x []     = undefined 
separatePixels w h pixels = take w pixels: separatePixels w (h-1) (drop w pixels) 


makeImage :: String -> IO Image
makeImage str = do
    let
        listStr = lines str
        wordsStr = words (listStr!!1) 
        w = read (head wordsStr)
        h = read (head (tail wordsStr))
        c = separatePixels w h (getPixels (drop 3 listStr))
    return (Image w h c)

loadImage :: String -> IO Image
loadImage path = do
            str <- readFile path 
            makeImage str 



-------------------------------------------------------------

--createMatrix :: Int -> Int -> Int -> Int -> [[a]] -> [a]
--createMatrix i j w h m = 
--    [ m !! (clamp (i+di) 0 h) !! (clamp (j+dj) 0 w) | di<-[-1..1], dj<-[-1..1] ] 
   


--clamp :: Ord a => a -> a -> a -> a
--clamp x a b = max a (min x b) 
 
-- \(x,y) -> (mod (x+h) h, mod (y+w) w) 