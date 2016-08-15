{-# LANGUAGE NamedFieldPuns #-}

import Data.Functor ((<$>))
import Control.Monad.State.Lazy

data Host
  = H
  { ip :: String
  , port :: Int }
    
instance Show Host where
  show (H {ip, port}) =
    ip ++ ":" ++ show port
    
data Status
  = Mapped
  | Local
  | Accept
  | Reject
  | Received
  deriving (Show)

mkStatus :: Packet -> Packet
mkStatus p@(P (H sip sp) (H dip dp) Received) =
  if isLocal sip && isLocal dip
  then (P (H sip sp) (H dip dp) Local)
  else p
  
isLocal :: String -> Bool
isLocal ip =
  "10.0.0.0" <= ip && ip <= "10.255.255.255"
  
data Packet
  = P
  { source :: Host
  , destination :: Host
  , status :: Status }

instance Show Packet where
  show (P s@(H sip sp) d@(H dip dp) status) =
    case status of
      Local -> show s ++ " " ++ show d ++ " local"
      Received -> show Received

parse :: String -> Packet
parse s =
  packet (words s)
  where
    packet :: [String] -> Packet
    packet (sip:sp:dip:dp:[]) =
      P (H sip (read sp)) (H dip (read dp)) Received
    packet _ = undefined

main :: IO ()
main = 
  do
    inputs <- getContents >>= return . lines
    let packets = parse <$> inputs
    let outputs = print . mkStatus <$> packets
    sequence_ outputs
