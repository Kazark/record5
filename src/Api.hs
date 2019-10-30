{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import           Servant.API
import qualified Servant.API as Api
import           Data.Record5 (Record5)
import           Data.Record5.Syntax (parseRecord5)
import           Network.HTTP.Media ((//))
import           Data.Delimited.Delimiter (Delimiter(..))
import           Data.Delimited (parseDelimitedLine)

type UserAPI =
  "records" :> ( PostNoContent '[CSV, PSV, SSV] Record5
                 :<|> "gender" :> Get '[JSON] [Record5]
                 :<|> "birthdate" :> Get '[JSON] [Record5]
                 :<|> "name" :> Get '[JSON] [Record5]
               )

-- | Comma-separated values
data CSV
-- | Pipe-separated values
data PSV
-- | Space-separated values
data SSV

instance Api.Accept CSV where
    contentType _ = "text" // "csv"

instance Api.Accept PSV where
    contentType _ = "text" // "psv"

instance Api.Accept SSV where
    contentType _ = "text" // "ssv"

instance MimeUnrender CSV Record5 where
    mimeUnrender _ raw = do
      delimited <- parseDelimitedLine Comma raw
      parseRecord5 delimited
