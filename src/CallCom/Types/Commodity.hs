{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Commodity
  ( Commodity (Commodity),
    CommodityDescription (CommodityDescription),
    CommodityId (CommodityId)
  ) where


import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.User (UserId)
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
  deriving (Generic)


newtype CommodityDescription =
  CommodityDescription
    { unCommodityDescription :: Map Text Text }
  deriving Generic


newtype CommodityId =
  CommodityId
    { unCommodityId :: Int }
  deriving Generic
