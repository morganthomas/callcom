module CallCom.TokenIssue (getTokenIssueId) where


import CallCom.Types.TokenIssue (TokenIssue, TokenIssueId (TokenIssueId))
import Codec.Serialise (serialise)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Lazy (toStrict)


getTokenIssueId :: TokenIssue -> TokenIssueId
getTokenIssueId = TokenIssueId . hash . toStrict . serialise
