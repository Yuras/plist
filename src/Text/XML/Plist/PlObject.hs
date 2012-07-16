-----------------------------------------------------------------------------
--
-- Module      :  Text.XML.Plist.PlObject
-- Copyright   :  (c) Yuras Shumovich 2009
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

-- |Data type that represents plist object
data PlObject =
    -- |string
    PlString String |
    -- |bool
    PlBool Bool |
    -- |integer
    PlInteger Int |
    -- |real
    PlReal Double |
    -- |array
    PlArray [PlObject] |
    -- |dictionary
    PlDict [(String, PlObject)] |
    -- |raw data
    PlData [Word8] |
    -- |date (ISO 8601, but currently it is not validated)
    PlDate String
    deriving Show

fromPlString :: Monad m => PlObject -> m String
fromPlString (PlString str) = return str
fromPlString o = fail $ "not a string: " ++ show o

fromPlBool :: Monad m => PlObject -> m Bool
fromPlBool (PlBool bool) = return bool
fromPlBool o = fail $ "not a bool: " ++ show o

fromPlInteger :: Monad m => PlObject -> m Int
fromPlInteger (PlInteger i) = return i
fromPlInteger o = fail $ "not an integer: " ++ show o

fromPlReal :: Monad m => PlObject -> m Double
fromPlReal (PlReal r) = return r
fromPlReal o = fail $ "not a real: " ++ show o

fromPlArray :: Monad m => PlObject -> m [PlObject]
fromPlArray (PlArray arr) = return arr
fromPlArray o = fail $ "not an array: " ++ show o

fromPlDict :: Monad m => PlObject -> m [(String, PlObject)]
fromPlDict (PlDict d) = return d
fromPlDict o = fail $ "not a dictionary: " ++ show o

