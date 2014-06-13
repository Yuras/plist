-----------------------------------------------------------------------------
--
-- Module      :  Text.XML.Plist.Write
-- Copyright   :  (c) Yuras Shumovich 2009, Michael Tolly 2012
-- License     :  BSD3
--
-- Maintainer  :  shumovichy@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- | Generating property list format
--
-----------------------------------------------------------------------------

module Text.XML.Plist.Write (

writePlistToFile,
objectToPlist,
objectToXml

) where

import Text.XML.Plist.PlObject
import Text.XML.HXT.Arrow.XmlState
import Codec.Binary.Base64
import Control.Monad (void)

import Control.Arrow.IOStateListArrow
import Text.XML.HXT.Arrow.WriteDocument
import Text.XML.HXT.Arrow.XmlArrow
import Control.Arrow
import Control.Arrow.ArrowList
import Text.XML.HXT.DOM.TypeDefs

-- | Write 'PlObject' to file
writePlistToFile :: String -> PlObject -> IO ()
writePlistToFile fileName object =
  void $ runX (constA object >>> writePlist fileName)

writePlist :: String -> IOSLA (XIOState s) PlObject XmlTree
writePlist fileName = objectToPlist >>>
  writeDocument [withIndent yes, withAddDefaultDTD yes] fileName

-- | Arrow to convert 'PlObject' to plist with root element and DTD declaration.
objectToPlist :: ArrowDTD a => a PlObject XmlTree
objectToPlist = root
  [ sattr "doctype-name" "plist"
  , sattr "doctype-SYSTEM" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"
  , sattr "doctype-PUBLIC" "-//Apple Computer//DTD PLIST 1.0//EN"
  ] [mkelem "plist" [sattr "version" "1.0"] [objectToXml $< this]]

-- | Arrow to convert 'PlObject' to XML. It produces 'XmlTree' without root
-- element.
objectToXml :: ArrowXml a => PlObject -> a b XmlTree
objectToXml (PlString str) = selem "string" [txt str]
objectToXml (PlBool bool) = eelem $ if bool then "true" else "false"
objectToXml (PlInteger int) = selem "integer" [txt $ show int]
objectToXml (PlReal real) = selem "real" [txt $ show real]
objectToXml (PlArray objects) = selem "array" $ map objectToXml objects
objectToXml (PlDict objects) = selem "dict" elems where
  elems = concatMap toXml objects
  toXml (key, val) = [selem "key" [txt key], objectToXml val]
objectToXml (PlData dat) = selem "data" [txt $ enc dat] where
  enc = (++ "\n") . foldr ((++) . ("\n" ++)) "" . chop 20 . encode
objectToXml (PlDate date) = selem "date" [txt date]
