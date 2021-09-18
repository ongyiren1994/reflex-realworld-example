{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, StandaloneDeriving, TypeOperators #-}
module Common.Conduit.Api.Profiles
  ( ProfilesApi
  , Profile(Profile)
  ) where

import Data.Text    (Text)
import Servant.API
import Servant.Auth (Auth, JWT)

import Common.Conduit.Api.Namespace        (Namespace)
import Common.Conduit.Api.Profiles.Profile

type ProfilesApi token = (Auth '[JWT] token :> Capture "username" Text :>  Get '[JSON] (Namespace "profile" Profile))
                         :<|> (Auth '[JWT] token :> QueryParam "username" Text :> QueryParam "follow" Bool :> GetNoContent '[JSON] NoContent )