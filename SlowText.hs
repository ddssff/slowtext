{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports #-}
{-# OPTIONS -Wall -ddump-rule-firings #-}

module SlowText (slowText) where

import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate)
--import Data.ByteString (ByteString)
--import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime, NominalDiffTime)

slowText :: (Show a, Monoid a, NFData a) => String -> [a] -> IO NominalDiffTime
slowText msg lst = do
  (_, t) <- timeTask $ return $ force $ test1 lst
  putStrLn $ "Elapsed: " ++ show t ++ " " ++ msg
  return t
    where
      test1 [] = mempty
      test1 (x : xs) = x `mappend` test1 xs
      -- test2 = foldr mappend mempty  -- Alternative to test1

timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)
