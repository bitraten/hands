{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving,
TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts,
FlexibleInstances #-}
module Model.Post where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.URI (URI)

import Database.Persist
import Database.Persist.TH
import Model.Fields

share [mkPersist sqlSettings, mkMigrate "migratePost"] [persistLowerCase|
Post json
    slug Text
    domain Text
    body Text
    body_html Text
--    created_at UTCTime
--    updated_at UTCTime
--    tags [Text]
--    published_at UTCTime
    guid Text
--    edited_at UTCTime
    url URI
    referenced_guid Text
    UniqueGuid guid
    deriving Show
|]
