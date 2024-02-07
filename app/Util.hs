module Util where

import RIO
import Data.ByteArray ( Bytes, convert)
import Data.Text.Encoding (decodeUtf8)


fromTextToBytes :: Text -> Bytes
fromTextToBytes = convert . encodeUtf8

fromBytesToTextUnsafe :: Bytes -> Text
fromBytesToTextUnsafe = decodeUtf8 . convert

