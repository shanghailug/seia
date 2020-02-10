module SHLUG.Seia.Conf ( confServ
                       , ConfOp(..)
                       , ConfRes(..)
                       ) where

import SHLUG.Seia.Rt

import Data.ByteString(ByteString(..))
import qualified Data.ByteString as BS

import Reflex
import Control.Monad.Fix (MonadFix)

import Control.Monad.Catch (MonadCatch, catch)
import Control.Exception (SomeException)

import Data.Set ( Set(..) )
import qualified Data.Set as Set

import Data.Map.Strict ( Map(..) )
import qualified Data.Map.Strict as Map

{-
every service save config under key "<serivce>/*"
and conf service itself store config under "conf/*"
-}

data ConfOp = ConfGet String |
              ConfSet String ByteString
              deriving (Show, Eq)
data ConfRes = ConfRes String (Maybe ByteString)
               deriving (Show, Eq)

confServ :: (Reflex t) => Event t [ConfOp] -> Event t ConfRes
confServ = undefined

confFilter :: (Reflex t) => (String -> Bool) -> Event t ConfRes ->
              ( Dynamic t (Map String ByteString)
              , Dynamic t (Set String)
              )
confFilter = undefined
