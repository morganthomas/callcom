module CallCom.Commodity (getCommodityId) where


import CallCom.Types.Commodity (CommodityId (CommodityId), CommodityDescription)
import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.User (UserId)
import Codec.Serialise (serialise)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Lazy (toStrict)
import Data.Set (Set)
import Data.Time (UTCTime)


getCommodityId :: Set CommodityTypeId -> Maybe CommodityDescription -> UTCTime -> UserId -> CommodityId
getCommodityId ts d c u =
  CommodityId . hash . toStrict
    $ serialise (ts, d, c, u)
