{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Asset
  ( Asset (Asset),
    AssetDescription (AssetDescription),
    AssetId (AssetId)
  ) where


import CallCom.Types.AssetClass (AssetClassId)
import CallCom.Types.User (UserId)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)


data Asset =
  Asset
    { assetClasses :: Set AssetClassId,
      id :: AssetId,
      description :: Maybe AssetDescription,
      owner :: UserId
    }
  deriving (Generic)


newtype AssetDescription =
  AssetDescription
    { unAssetDescription :: Text }
  deriving Generic


newtype AssetId =
  AssetId
    { unAssetId :: Int }
  deriving Generic
