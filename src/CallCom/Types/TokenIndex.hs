{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.TokenIndex
  ( TokenIndex (TokenIndex),
    TokenIndexName (TokenIndexName),
    TokenIndexId (TokenIndexId),
    ComponentAllocation (ComponentAllocation),
    AllocationWeight (AllocationWeight),
    ConversionRatio (ConversionRatio)
  ) where


import CallCom.Types.TokenBalance (TokenBalance)
import CallCom.Types.TokenIssue (TokenIssueId)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


data TokenIndex =
  TokenIndex
    { id :: TokenIndexId,
      name :: TokenIndexName,
      components :: Map TokenIssueId ComponentAllocation,
      created :: UTCTime
    }
  deriving Generic


newtype TokenIndexId =
  TokenIndexId
    { unTokenIndexId :: ByteString }
  deriving Generic


newtype TokenIndexName =
  TokenIndexName
    { unTokenIndexName :: Text }
  deriving Generic


data ComponentAllocation =
  ComponentAllocation
    { weight :: AllocationWeight,
      ratio :: ConversionRatio
    }
  deriving Generic


-- The number of component tokens allocated to the index
data AllocationWeight =
  AllocationWeight
    { unAllocationWeight :: TokenBalance
    }
  deriving Generic


-- The number of index tokens per component token
data ConversionRatio =
  ConversionRatio
    { unConversionRatio :: Rational
    }
  deriving Generic
