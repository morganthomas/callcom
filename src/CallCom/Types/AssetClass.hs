{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.AssetClass
  ( AssetClass (AssetClass),
    AssetClassId (AssetClassId),
    AssetClassName (AssetClassName)
  ) where


import Data.Text (Text)
import GHC.Generics (Generic)


data AssetClass =
  AssetClass
    { id :: AssetClassId,
      name :: AssetClassName
    }
  deriving Generic


newtype AssetClassId =
  AssetClassId
    { unAssetClassId :: Int }
  deriving Generic


newtype AssetClassName =
  AssetClassName
    { unAssetClassName :: Text }
  deriving Generic
