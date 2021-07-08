-----------------------------------------------------------------------------
--
-- Module      :  Text.XML.Plist.PlObject
-- Copyright   :  (c) Yuras Shumovich 2009, Michael Tolly 2012
-- License     :  BSD3
--
-- Maintainer  :  shumovichy@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- |PlObject data type
--
-----------------------------------------------------------------------------

module Text.XML.Plist.PlObject (

PlObject(..),
fromPlString,
fromPlBool,
fromPlInteger,
fromPlReal,
fromPlArray,
fromPlDict

) where

import Data.Word

-- | Data type that represents plist object
data PlObject
  = PlString String -- ^ string
  | PlBool Bool -- ^ bool
  | PlInteger Int -- ^ integer
  | PlReal Double -- ^ real
  | PlArray [PlObject] -- ^ array
  | PlDict [(String, PlObject)] -- ^ dictionary
  | PlData [Word8] -- ^ raw data
  | PlDate String -- ^ date (ISO 8601, but currently it is not validated)
  deriving (Eq, Ord, Show, Read)

fromPlString :: MonadFail m => PlObject -> m String
fromPlString (PlString str) = return str
fromPlString o = fail $ "not a string: " ++ show o

fromPlBool :: MonadFail m => PlObject -> m Bool
fromPlBool (PlBool bool) = return bool
fromPlBool o = fail $ "not a bool: " ++ show o

fromPlInteger :: MonadFail m => PlObject -> m Int
fromPlInteger (PlInteger i) = return i
fromPlInteger o = fail $ "not an integer: " ++ show o

fromPlReal :: MonadFail m => PlObject -> m Double
fromPlReal (PlReal r) = return r
fromPlReal o = fail $ "not a real: " ++ show o

fromPlArray :: MonadFail m => PlObject -> m [PlObject]
fromPlArray (PlArray arr) = return arr
fromPlArray o = fail $ "not an array: " ++ show o

fromPlDict :: MonadFail m => PlObject -> m [(String, PlObject)]
fromPlDict (PlDict d) = return d
fromPlDict o = fail $ "not a dictionary: " ++ show o
