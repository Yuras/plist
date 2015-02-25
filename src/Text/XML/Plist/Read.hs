-----------------------------------------------------------------------------
--
-- Module      :  Text.XML.Plist.Read
-- Copyright   :  (c) Yuras Shumovich 2009, 2012, Michael Tolly 2012
-- License     :  BSD3
--
-- Maintainer  :  shumovichy@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- | Parsing property list format
--
-----------------------------------------------------------------------------

module Text.XML.Plist.Read (

readPlistFromFile,
readPlistFromString,
plistToObject,
xmlToObject

) where

import Text.XML.Plist.PlObject
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree
import Text.XML.HXT.DOM.TypeDefs
import Control.Arrow.ArrowIf
import Data.ByteString.Char8 (pack)
import Data.ByteString (unpack)
import Data.ByteString.Base64
import Data.Maybe

-- | Read 'PlObject' from file.
readPlistFromFile :: SysConfigList -> String -> IO PlObject
readPlistFromFile opts fileName = do
  res <- runX $ readDocument opts fileName >>> plistToObject
  case res of
    [] -> fail $ "can't parse " ++ fileName
    (x:_) -> return x

-- | Read 'PlObject' from string.
readPlistFromString :: SysConfigList -> String -> IO PlObject
readPlistFromString opts string = do
  res <- runX $ readString opts string >>> plistToObject
  case res of
    [] -> fail $ "can't parse string " ++ string
    (x:_) -> return x

-- | Arrow that converts XML tree to 'PlObject'.
-- Tree should contain at list one \"plist\" element.
plistToObject :: ArrowXml a => a XmlTree PlObject
plistToObject = deep (hasName "plist") >>> getChildren >>> xmlToObject

-- | Arrow that converts XML element to 'PlObject'.
-- Element should be \"string\", \"array\", \"dict\", etc.
xmlToObject :: ArrowXml a => a XmlTree PlObject
xmlToObject = choiceA
  [ hasName "string" :-> (innerText >>> arr PlString)
  , hasName "true" :-> constA (PlBool True)
  , hasName "false" :-> constA (PlBool False)
  , hasName "integer" :-> (innerText >>> arr (PlInteger . read))
  , hasName "real" :-> (innerText >>> arr (PlReal . read))
  , hasName "array" :-> (listA readArray >>> arr PlArray)
  , hasName "dict" :-> (readDict >>> arr PlDict)
  , hasName "data" :-> (
    innerText >>>
    arr (decode' . foldr (++) "" . lines) >>>
    isA isJust >>>
    arr fromJust >>>
    arr PlData
    )
  , hasName "date" :-> (innerText >>> arr PlDate) ]
  where
    decode' = either (const Nothing) Just . fmap unpack . decode . pack

readDict :: ArrowXml a => a XmlTree [(String, PlObject)]
readDict = listA $ readDict' $< listA (getChildren >>> isElem)

readDict' :: ArrowXml a => [XmlTree] -> a b (String, PlObject)
readDict' (key : val : xs) =
  ((constA key >>> hasName "key" >>> innerText) &&&
  (constA val >>> xmlToObject)) <+>
  readDict' xs
readDict' _ = none

readArray :: ArrowXml a => a XmlTree PlObject
readArray = getChildren >>> xmlToObject

innerText :: ArrowXml a => a XmlTree String
innerText = withDefault (single (getChildren >>> isText >>> getText)) ""
