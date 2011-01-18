module Main where
import Graphics.UI.WX
import System.Random
import Data.Array.IArray

-- constants --

areaSize :: (Int,Int)
areaSize = (100, 80)

areaBounds :: ((Int,Int), (Int,Int))
areaBounds = ((1,1), areaSize)

cellSize :: Int
cellSize = 8

direction :: [(Int,Int)]
direction = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

speed :: Int
speed = 100

-- types --

data Cell = Guard | Empty | Life
    deriving (Eq, Show)

type Area = Array (Int,Int) Cell

newtype Generation = Gen [(Int, Area)]

generation :: Area -> Generation
generation base = Gen $ zip [1..] (genSeq base)

currentStep :: Generation -> Int
currentArea :: Generation -> Area
nextGen :: Generation -> Generation

currentStep (Gen ((s,_):_)) = s
currentArea (Gen ((_,a):_)) = a
nextGen (Gen (_:xs)) = Gen xs

-- main function --

main :: IO ()
main = start lifeGameFrame

lifeGameFrame =
 do vArea <- randomGeneration >>= varCreate
    
    f <- frameFixed [ text := "Conway's Life Game Simulator" ]
    p <- panel f [ on paint := paintArea vArea ]
    s <- staticText f [ text := "1" ]
    t <- timer f [ interval := speed
                 , on command := nextArea vArea (do repaint p
                                                    v <- varGet vArea
                                                    set s [ text := show $ currentStep v ])
                 , enabled := False ]
    
    buttons <- sequence [
          button f [ text := "시작/일시정지"
                   , on command := set t [ enabled :~ not ] ]
        , button f [ text := "재시작"
                   , on command := randomGeneration >>= varSet vArea
                                >> set t [ enabled := True ] ]
        ]

    let (aw,ah) = areaSize
        stepTextLayout = floatLeft  . margin 8 . row 4 $ [ label "세대:", widget s ]
        buttonsLayout  = floatRight . margin 4 . row 4 $ map widget buttons
        topBarLayout   = row 8 [ stepTextLayout, buttonsLayout ]
        panelLayout    = minsize (sz (aw*cellSize) (ah*cellSize)) $ widget p
    
    set f [ layout := column 0 [ topBarLayout, panelLayout ] ]
    
  where paintArea :: Var Generation -> DC a -> Rect -> IO ()
        paintArea vArea dc viewArea =
         do area <- varGet vArea >>= return . currentArea
            set dc [ brushColor := yellow, brushKind := BrushSolid ]
            mapM_ (drawCell dc) [ix | ix <- range areaBounds, area!ix == Life]
        
        ix2Rect (x,y) = rect p s
          where p = Point (coord x) (coord y)
                s = let a = cellSize-2 in sz a a
                coord x = (x-1) * cellSize + 1
        
        drawCell dc ix = drawRect dc (ix2Rect ix) []
        
        nextArea :: Var Generation -> IO () -> IO ()
        nextArea vArea act = varUpdate vArea nextGen >> act


randomGeneration :: IO Generation
randomGeneration = newStdGen >>= return . generation . initAreaWithRandom

genSeq :: Area -> [Area]
genSeq area = area : genSeq nextGen
  where nextGen = area // [ (ix, nextState (area!ix) (liveNeighbors ix)) | ix <- range areaBounds]
        liveNeighbors (x,y) = length [(x+dx,y+dy) | (dx,dy) <- direction, area!(x+dx,y+dy) == Life]
        nextState Life 2 = Life
        nextState _    3 = Life
        nextState _    _ = Empty

-- create an area with random-generated lives.
initAreaWithRandom :: RandomGen g => g -> Area
initAreaWithRandom g = array guardedRange $ zipWith cell (range guardedRange) seq
  where (w,h) = areaSize
        guardedRange = ((0,0), (w+1,h+1))

        cell ix val = (ix, if inRange areaBounds ix then val else Guard)

        seq = [if x then Life else Empty | x <- randoms g]
