{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.CommodityType
  ( CommodityType (CommodityType),
    CommodityTypeId (CommodityTypeId),
    CommodityTypeName (CommodityTypeName)
  ) where


import Data.Text (Text)
import GHC.Generics (Generic)


data CommodityType =
  CommodityType
    { id :: CommodityTypeId,
      name :: CommodityTypeName
    }
  deriving Generic


newtype CommodityTypeId =
  CommodityTypeId
    { unCommodityTypeId :: Int }
  deriving Generic


newtype CommodityTypeName =
  CommodityTypeName
    { unCommodityTypeName :: Text }
  deriving Generic
