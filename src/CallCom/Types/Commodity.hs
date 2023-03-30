{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.Commodity
  ( Commodity (Commodity),
    CommodityDescription (CommodityDescription),
    CommodityId (CommodityId)
  ) where


import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.User (UserId)
import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)


data Commodity =
  Commodity
    { id :: CommodityId,
      types :: Set CommodityTypeId,
      description :: Maybe CommodityDescription,
      owner :: UserId
    }
  deriving (Eq, Ord, Generic, Show)

instance Serialise Commodity


newtype CommodityDescription =
  CommodityDescription
    { unCommodityDescription :: Map Text Text }
  deriving (Eq, Ord, Generic, Show, Serialise)


newtype CommodityId =
  CommodityId
    { unCommodityId :: ByteString }
  deriving (Eq, Ord, Generic, Show, Serialise)
