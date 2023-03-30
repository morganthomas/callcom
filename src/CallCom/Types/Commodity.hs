{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Commodity
  ( Commodity (Commodity),
    CommodityDescription (CommodityDescription),
    CommodityId (CommodityId)
  ) where


import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.User (UserId)
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


newtype CommodityDescription =
  CommodityDescription
    { unCommodityDescription :: Map Text Text }
  deriving (Eq, Ord, Generic, Show)


newtype CommodityId =
  CommodityId
    { unCommodityId :: ByteString }
  deriving (Eq, Ord, Generic, Show)
