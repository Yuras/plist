-----------------------------------------------------------------------------
--
-- Module      :  Text.XML.Plist.Read
-- Copyright   :  (c) Yuras Shumovich 2009
-- License     :  BSD3
--
-- Maintainer  :  shumovichy@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- |Parsing property list format
--
-----------------------------------------------------------------------------

module Text.XML.Plist.Read (

readPlistFromFile,
plistToObject,
xmlToObject

) where

import Text.XML.Plist.PlObject
import Text.XML.HXT.Arrow
import Codec.Binary.Base64
import Data.Maybe

-- |Read 'PlObject' from file.
readPlistFromFile :: String -> IO PlObject
readPlistFromFile fileName =
    do
    res <- runX (
        readDocument [] fileName >>>
        plistToObject
        )
    if null res
        then fail $ "can't parse " ++ fileName
        else return $ head res

-- |Arrow that converts xml tree to 'PlObject'.
-- Tree should contain at list one \"plist\" element.
plistToObject :: ArrowXml a => a XmlTree PlObject
plistToObject =
    deep (hasName "plist") >>>
    getChildren >>>
    xmlToObject

-- |Arrow that converts xml element to 'PlObject'.
--  Element should be \"string\", \"array\", \"dict\", etc.
xmlToObject :: ArrowXml a => a XmlTree PlObject
xmlToObject = choiceA [
    hasName "string" :-> (innerText >>> arr PlString),
    hasName "true" :-> constA (PlBool True),
    hasName "false" :-> constA (PlBool False),
    hasName "integer" :-> (innerText >>> arr (PlInteger . read)),
    hasName "real" :-> (innerText >>> arr (PlReal . read)),
    hasName "array" :-> (listA readArray >>> arr PlArray),
    hasName "dict" :-> (readDict >>> arr PlDict),
    hasName "data" :-> (
        innerText >>>
        arr (decode . unchop . lines) >>>
        isA isJust >>>
        arr fromJust >>>
        arr PlData
        ),
    hasName "date" :-> (innerText >>> arr PlDate)
    ]

readDict :: ArrowXml a => a XmlTree [(String, PlObject)]
readDict = listA $
    readDict' $< listA (getChildren >>> isElem)

readDict' :: ArrowXml a => [XmlTree] -> a b (String, PlObject)
readDict' [] = none
readDict' [_] = none
readDict' (key : val : xs) =
    ((constA key >>> hasName "key" >>> innerText) &&&
    (constA val >>> xmlToObject)) <+>
    readDict' xs

readArray :: ArrowXml a => a XmlTree PlObject
readArray =
    getChildren >>>
    xmlToObject

innerText :: ArrowXml a => a XmlTree String
innerText = withDefault (single (
    getChildren >>>
    isText >>>
    getText
    )) ""

