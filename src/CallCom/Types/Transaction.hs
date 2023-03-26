{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Transaction
  ( Transaction (Transaction)
  ) where


import CallCom.Types.Positions (Positions)
import CallCom.Types.User (UserId)
import Data.Map (Map)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)


data Transaction =
  Transaction
    { inputs :: Map UserId Positions,
      outputs :: Map UserId Positions,
      timestamp :: UTCTime
    }
  deriving Generic
