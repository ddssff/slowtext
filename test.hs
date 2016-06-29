{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -ddump-rule-firings #-}

import Data.ByteString (ByteString)
import Data.Text (Text)
import SlowText
import System.Exit

main :: IO ()
main = do
  t1 <- slowText' 10000000 "String" ("Hello" :: String)
  t2 <- slowText' 10000 "ByteString" ("Hello" :: ByteString)
  t3 <- slowText' 10000 "Text" ("Hello" :: Text)
  let ratios = [t2 / t1, t3 / t1]
  putStrLn ("# times slower than String: " ++ show ratios)
  exitWith $ case any (< 1.0) ratios of
               False -> ExitFailure 1
               True -> ExitSuccess
    where
      -- Return the average amount of time spent processing each Char
      slowText' count label x = do
         elapsed <- slowText (show count ++ " " ++ label) (replicate count x)
         return $ elapsed / fromInteger (toInteger count)
