module CallCom.JSON (base64Parser, base64ToJSON) where


import Data.Aeson (Value (String), FromJSON (parseJSON))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)


base64Parser :: Value -> Parser ByteString
base64Parser x =
   either
    (fail . unpack)
    pure
      =<< (decodeBase64 . encodeUtf8
            <$> parseJSON x)


base64ToJSON :: ByteString -> Value
base64ToJSON = String . encodeBase64
