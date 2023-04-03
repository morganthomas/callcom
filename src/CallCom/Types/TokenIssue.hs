{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.TokenIssue
  ( TokenIssue (TokenIssue),
    TokenIssueId (TokenIssueId),
    TokenFraction (TokenFraction)
  ) where


import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.TokenBalance (TokenBalance)
import CallCom.Types.User (UserId)
import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


data TokenIssue =
  TokenIssue
    { underlying :: CommodityTypeId,
      fraction :: TokenFraction,
      created :: UTCTime,
      issuers :: Set UserId,
      circulation :: TokenBalance
    }
  deriving (Eq, Ord, Generic, Show)

instance Serialise TokenIssue


newtype TokenIssueId =
  TokenIssueId
    { unTokenIssueId :: ByteString }
  deriving (Eq, Ord, Generic, Show, Serialise)


-- The number of tokens per unit of underlying asset
newtype TokenFraction =
  TokenFraction
    { unTokenFraction :: Int }
  deriving (Eq, Ord, Generic, Show, Serialise)
