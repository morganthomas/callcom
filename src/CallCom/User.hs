module CallCom.User (getUserId) where


import CallCom.Types.User (UserName, UserId (UserId))
import Codec.Serialise (serialise)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Lazy (toStrict)
import Data.Time (UTCTime)


getUserId :: UserName -> UTCTime -> UserId
getUserId = curry (UserId . hash . toStrict . serialise)
