-----------------------------------------------------------------------------
--
-- Module      :  Text.XML.Plist.Write
-- Copyright   :  (c) Yuras Shumovich 2009
-- License     :  BSD3
--
-- Maintainer  :  shumovichy@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- |Generating property list format
--
-----------------------------------------------------------------------------

module Text.XML.Plist.Write (

writePlistToFile,
objectToPlist,
objectToXml

) where

import Text.XML.Plist.PlObject
import Text.XML.HXT.Arrow
import Codec.Binary.Base64

-- |Write 'PlObject' to file
writePlistToFile :: String -> PlObject -> IO ()
writePlistToFile fileName object = runX (constA object >>> writePlist fileName) >> return ()

writePlist :: String -> IOSLA (XIOState s) PlObject XmlTree
writePlist fileName =
    objectToPlist >>>
    writeDocument [(a_indent, "1"), (a_add_default_dtd, "1")] fileName

-- |Arrow to convert 'PlObject' to plist with root element and DTD declaration.
objectToPlist :: ArrowDTD a => a PlObject XmlTree
objectToPlist = root [
        sattr "doctype-name" "plist",
        sattr "doctype-SYSTEM" "http://www.apple.com/DTDs/PropertyList-1.0.dtd",
        sattr "doctype-PUBLIC" "-//Apple Computer//DTD PLIST 1.0//EN"
    ] [mkelem "plist" [sattr "version" "1.0"] [objectToXml $< this]]

-- |Arrow to convert 'PlObject' to xml. It produces 'XmlTree' without root element.
objectToXml :: ArrowXml a => PlObject -> a b XmlTree
objectToXml (PlString str) = selem "string" [txt str]
objectToXml (PlBool bool) = eelem $ if bool then "true" else "false"
objectToXml (PlInteger int) = selem "integer" [txt $ show int]
objectToXml (PlReal real) = selem "real" [txt $ show real]
objectToXml (PlArray objects) = selem "array" $ map objectToXml objects
objectToXml (PlDict objects) = selem "dict" elems
    where
    elems = concatMap toXml objects
    toXml (key, val) = [selem "key" [txt key], objectToXml val]
objectToXml (PlData dat) = selem "data" [txt $ enc dat]
    where
    enc = (++ "\n") . foldr ((++) . ("\n" ++)) "" . chop 20 . encode
objectToXml (PlDate date) = selem "date" [txt date]

