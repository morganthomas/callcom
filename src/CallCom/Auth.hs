{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}


module CallCom.Auth
  ( getLoginPasswordHash,
    getSpendingPasswordHash,
    getUserKeyPair,
    getSignature,
    verifySignature
  ) where


import CallCom.Types.Auth (LoginPassword, LoginPasswordHash (LoginPasswordHash), Signature (Signature), SpendingPassword, SpendingPasswordHash (SpendingPasswordHash), UserPrivateKey (UserPrivateKey), UserPublicKey (UserPublicKey))
import Codec.Serialise (Serialise, serialise)
import Control.Arrow ((***))
import Control.Lens ((^.))
import Crypto.Hash.SHA256 (hash)
import Crypto.Sign.Ed25519 (createKeypairFromSeed_, sign, verify)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Generics.Labels ()


newtype Salt = Salt ByteString


saltedHash :: Serialise a => Salt -> a -> ByteString
saltedHash (Salt s) = hash . (<> s) . toStrict . serialise


getLoginPasswordHash :: LoginPassword -> LoginPasswordHash
getLoginPasswordHash =
  LoginPasswordHash . saltedHash loginSalt . (^. #unLoginPassword)
  where
    loginSalt = Salt $ hash "v8jOAVciGKsUG80Zv8pV95hhSfIT0VzYlqfZEdVdCxduigFcZ0nMgJ6i23UdCtSsoXu9QNX+ldAVS6SDVWnAfNjnoKSe9pqlYLZb2Vf4zP+wsqCwZmfVTT0pQzkvjoICxIoSpkb+oQCEwzY19b3qSSLy31ckh0decWlKxwkO5zKWNTFBDbSPg680f9PMFbm+hP23ck1XrA5wFnt9xbhKFIO2ty4dqAQAnKDwrW4FPjpy6JtyRpyxBt6jC4vflYwnfvk0jNhhhR9ayOt8TrK4pqCxqIQ/aPtjRl1SmT+Qgatltz4sJuay5OcBIBeAjHE5LO2TDyx1drTW24L0G4EYOyCS0sNOJ2S53eEBM3SftSIEY5l0ZYYhGMFgyzkD76Bzwmqv6kqNNaMpj8iEljElXdyWCcynMQlGAYZVPPyEu6xhAaLsI9T6NbNMOh9G88EUP4WrEwiZdhFnZQ3JWEimhKS5S8h36iOBbrK6vIVPxikTMB0E+tCIV5ty36WEDmrlfIxPQZ4D0GPeB9HXZszyTmyl8Uef7k/ALjwhmbz9AgpEufWWbTzsETTM1Mx8HyEgusQseBNjLXR6ZgebMGKJfVTjIYgk1W/eq+vdf4+aC0vyAw7ef12Fd9zHuGr7ok47UZjRiEM4Hko6/iH/Pzuyo8+HP5Bz04IoBzRjpTK5XBsGvXe2NF2vWoYDc8Uu5vMaS1DKnJuB+gaKBacVpXhwMiUtoZImv1JY8KwCrs2scwmORncIkhFQ95kti7owfXqJ65XUxZ+S0poK7VESxn1RoftM+nif4GqbKuJ2z6jJ3Mmz5NH/sNaSBLFX/xCb/vy5xhJky8qgHIjznOC7mk9AkOGYDkatu8CsjW57rawmYMidq8RXEz57uy63Mo9J/zWDKozsAph9e/qGhHlr4Ej23M5r2SVnD/jZHUgfaulALaouf0NinXMQCiNWTtmM8ChUTyUd3n28XMdOe+iYeoQ2yMyWL2RQI3XLsXyZfuiTCcojvbzYcE2FewU53NbHJkqg+cA9oeZdW3U9ApENhkKskJ8NRW117asO2u8aOIuBRaVHm/8yT8WdNKeaOeZ/gPTvDF0YgByrAVensl44p2s9hHNUpD8w+iEaop4SkAV0PSib8aafQt5OYZb0Dlb9XViaJtHw2WcL/SKkujAgZui6J7DIryiQYorP+wuHiRgpeRxgFHr3nKEO7BQdcdxt1t7uXpr3Z2jgBp5woUxbfvWojoiuhdSIJQGhc6GWxgECbSEqsThK4eHYF4iDmrsSNhbV0wfvlsmdFoMmOfOguXM1LROpWg/KR8l/7Zf3HsCQR5OmGr0CLcQialQ4NZkXEQMPMGF+Yow3VLI1kDFzEFew46yF430wmIHP+GTobdCRRd5yzOlxz5Ucg1YqojhArgG0l+bOL056R0lvyc+2LiUtEMl2y/I296WJnfdvm7FX1YxkV0rxjqxE4fe/1Q4arPwpU0l3DH8HaL9voTbdUJ5K/fxRMuQlsXBU1mpjh5+Rg7vzSAEH0puiFPNW9HnEgCIfkzhuQ8p3llC098rjMmgYvpVUTBphf"


getSpendingPasswordHash :: SpendingPassword -> SpendingPasswordHash
getSpendingPasswordHash =
  SpendingPasswordHash . saltedHash spendingSalt . (^. #unSpendingPassword)
  where
    spendingSalt = Salt $ hash "cSiuzZ6WLcMR9Zr2J40QMdA8GOe9gBTbZam0Jfrh+5dYzlNR/DJ6kaaGOphl3I6l0HTI1qmX+OJ7JDuotB6/SUgIVIa6SgtQP7Uozib19UK2bpC7Io0Qk8kCBsVl1VGINTiOqoV2Fssanm/a6WfyOcwCCgrEKZ35y6S8yrckbkCSkOZ0v7tVYqEE+azgC6ZUQi43LTmeiDuezgCqaxchgfANokAsQD1Wvz82S+k1mgWncTPLPQp5Ngf+zLhX0Hst4wPCB8Qe5GhYWKeVH8gOeJc604De6tuSn0URPs8irAdZuAH1epFQIFVk/Gnzl9WD4Ww/Cn53caoQh2FBiO2f3YRegkcdpGPQJ2Q6WreLccfW74bvODzkO8m4NUFKcuFIoi8PF5Xz0s0flDeYO6DuBwhnCJ29HXoPk0EDGU7Eh9/ljFtd6Hy0Ex5ghxHIlKJWvjC0slYAjMK3UQQkidnaYIBFbsgmv6EcyAuQk6ucLDw+JuLWdXWSQMwPtrQyRYFfMQt36q+Kn17kNFPzSngijieBEmPxGzd4cXnrK8HeUQR8tMJS+ms35gabN8SCRu+1wsimQroYsYaQqcukC6lWs72ile/LVRtShWzZeE7ZvKiIAr0KvIJe6wg1+/dauDtZTR/TOk9EAtk1Y2hb9GwprPTRN9wikI1EA7LK5IPB+DLlO9Bu9QMAnoCyaMoEvrsYqXeCNpPnyuJoGL11N10FeFl6esJQuv7ETaLJHPWDU6mh+VMNAWJ+EoqnY1SZsLQiQnWeUbVL/dB0DWSHtzA5pdf2BxuOp4Nwvt4KE1vbVW39ddR//tqVw5d7OzMjndv8xE0WWFrhbIyChcfQN5azNEDT3P32Pbm4B8AFTZyZe8R+On8/4OYZ7oykSZnpswowzamrX34mTa+3K+ZyoK6Xe99HPE61hBsOSfxElLZoEuEFuB2Dx9GIoDSOOOQjOWLr9n7qvg+yOLehHlrqAerlKmmhcH8s10IIVi+IWdBybnpF6wr/ns8bLCIgsKl0QtF6TGZ/Uq+XsodMqXEhM1pEM7Zx+mm70t2JZ87lI4gM8LcBhHwLLeZ4Waoc7N8DHQ9hmV/qNRri1gwScycKCWNEHVctMmQ8uNTbtVagdoP4YeY7D4A3B8TmAcMSZkFAqur9yRYOOCPjtvlHx+6ziW9ttGtjsw3qdouypaKeT9B8SkQI+IeUm0tMNWuokHKtmI4O9qMysH5SDQUcLWZb53+suX17QNGX9Gz3dolypXtJySmGdYyN1LmC4A/5zPhjc90o4LSdYNLvoU6PApSdg+GXKKCsMs4VeMbpQ1fZY+inrXnQ1v5AviYGTkeTQiOfS9k5/y0ArnatORVdXjKGGXQQMXsVDKc0O8cRYlFE9HuTLFxdxhT9nSHuNUqRvwC0ClJvmH8v2pGvInDBqcwZ1hu4arZvuIlIGt9RXgpRLKYAtd2B0O3oQzX3oi3eDbJ3bkPBrJCknBHvU/2mOxxvO9pF51J7qO8BN8PTUx3OOwQhHmJCzOd"


getUserKeyPair :: SpendingPasswordHash -> Maybe (UserPublicKey, UserPrivateKey)
getUserKeyPair =
  fmap (UserPublicKey *** UserPrivateKey) .
    createKeypairFromSeed_ . (^. #unSpendingPasswordHash)


getSignature :: Serialise a => UserPrivateKey -> a -> Signature
getSignature k =
  Signature . sign (k ^. #unUserPrivateKey) . toStrict . serialise


verifySignature :: Serialise a => UserPublicKey -> a -> Signature -> Bool
verifySignature k x (Signature sig) =
  verify (k ^. #unUserPublicKey)
    $ toStrict (serialise x) <> sig
