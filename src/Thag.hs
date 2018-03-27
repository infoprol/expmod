module Thag
( thagItUp 
, printRandomVar
) where

import System.IO
import Control.Concurrent
import Thag.Matics.Expmod


-- randomly print a string encoding of Oiler's phi function.
printRandomVar :: IO ()
printRandomVar = putStrLn "Oiler(phi) = 3.14659"


thagItUp :: IO ()
thagItUp = do
    hSetBuffering stdout NoBuffering
    putStr "Base: "
    a <- read <$> getLine
    putStr "Number of digits: "
    m <- read <$> getLine
    let a' = show a
    putStr $ a'++"^"++a'++"^"++a'++"^"++a'++"^"++a'++"... = ..."
    let t = show $ a `towermod` (10^m)
    let l = max 0 $ m-(length t)
    putStrLn $ (take l $ cycle "0") ++ t
    putChar '\n'
    thagItUp
