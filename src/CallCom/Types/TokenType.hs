{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.TokenType
  ( TokenType (TokenType),
    TokenTypeId (TokenTypeId),
    TokenFraction (TokenFraction)
  ) where


import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.TokenBalance (TokenBalance)
import CallCom.Types.User (UserId)
import Data.ByteString (ByteString)
import Data.Set (Set)
import GHC.Generics (Generic)


data TokenType =
  TokenType
    { id :: TokenTypeId,
      underlying :: CommodityTypeId,
      fraction :: TokenFraction,
      issuers :: Set UserId,
      circulation :: TokenBalance
    }
  deriving Generic


newtype TokenTypeId =
  TokenTypeId
    { unTokenTypeId :: ByteString }
  deriving Generic


-- The number of tokens per unit of underlying asset
newtype TokenFraction =
  TokenFraction
    { unTokenFraction :: Int }
  deriving Generic
