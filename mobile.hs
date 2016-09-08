module Haskell.Codewars.KeypadEntry where

import Data.List
  (find
  ,elem
  ,elemIndex)
import Control.Applicative
  ((<|>)
  ,(<$>))
import Data.Maybe
  (fromMaybe)
import Data.Char
  (toUpper)

keypad :: [(Char,String)]
keypad =
    [('1',"")
    ,('2',"ABC")
    ,('3',"DEF")
    ,('4',"GHI")
    ,('5',"JKL")
    ,('6',"MNO")
    ,('7',"PQRS")
    ,('8',"TUV")
    ,('9',"WXYZ")
    ,('*',"") 
    ,('0'," ")
    ,('#',"")]
  
encode :: Char -> String
encode c =
  fromMaybe "" (isKey c <|> findKey c)

isKey :: Char -> Maybe String
isKey c = 
  do
    found <- find ((==c) . fst) keypad
    let presses = ((+1) . length . snd $ found) 
    return $ replicate presses c
    
findKey :: Char -> Maybe String
findKey c =
  do
    found <- find (elem c . snd) keypad
    let presses = fromMaybe 0 $ (+1) <$> (elemIndex c . snd $ found)
    return $ replicate presses c        

presses :: String -> Int
presses =
  length . concatMap (encode . toUpper)
