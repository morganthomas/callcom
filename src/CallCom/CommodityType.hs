module CallCom.CommodityType (getCommodityTypeId) where


import CallCom.Types.CommodityType (CommodityType, CommodityTypeId (CommodityTypeId))
import Codec.Serialise (serialise)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Lazy (toStrict)


getCommodityTypeId :: CommodityType -> CommodityTypeId
getCommodityTypeId = CommodityTypeId . hash . toStrict . serialise
