-----------------------------------------------------------------------------
--
-- Module      :  Text.XML.Plist
-- Copyright   :  (c) Yuras Shumovich 2009
-- License     :  BSD3
--
-- Maintainer  :  shumovichy@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- |Library for generation and parsing Mac OS X plist format
--
-----------------------------------------------------------------------------

module Text.XML.Plist (

PlObject(..),
writePlistToFile,
readPlistFromFile,
objectToPlist,
plistToObject,
objectToXml,
xmlToObject,
fromPlString,
fromPlBool,
fromPlInteger,
fromPlReal,
fromPlArray,
fromPlDict

) where

import Text.XML.Plist.PlObject
import Text.XML.Plist.Read
import Text.XML.Plist.Write
