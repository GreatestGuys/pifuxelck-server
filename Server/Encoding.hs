module Server.Encoding (
    base64ToByteString
,   base64ToInteger
,   byteStringToBase64
,   bytesToInteger
,   integerToBytes
) where

import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

integerToBytes :: Integer -> BS.ByteString
integerToBytes = LBS.toStrict . Binary.encode

bytesToInteger :: BS.ByteString -> Integer
bytesToInteger = Binary.decode . LBS.fromStrict

base64ToInteger :: T.Text -> Integer
base64ToInteger = foldl (\a b -> a * 256 + fromIntegral b) 0
    . BS.unpack  -- ByteString -> Word8
    . base64ToByteString

base64ToByteString :: T.Text -> BS.ByteString
base64ToByteString = Base64.decodeLenient . T.encodeUtf8

byteStringToBase64 :: BS.ByteString -> T.Text
byteStringToBase64 = T.decodeUtf8 . Base64.encode
