module SHLUG.Seia.Helper where

import Data.Time.Clock.POSIX (getPOSIXTime)

-- show short string
sss :: Show a => Int -> a -> String
sss l x = if length a <= l then a else take (l - 4) a ++ " ..." where
  a = show x

getEpochMs :: Integral a => IO a
getEpochMs = do
  t <- getPOSIXTime
  return $ floor $ t * 1000
