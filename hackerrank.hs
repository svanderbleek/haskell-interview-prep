-- Repeat inputs N times

import Control.Monad (replicateM_, liftM)

main :: IO ()
main =
  do
    n <- readLn
    inputs >>= mapM_ (printN n)
    where
      inputs :: IO [String]
      inputs = liftM lines getContents
      printN :: Int -> String -> IO ()
      printN n s = replicateM_ n (putStrLn s)
