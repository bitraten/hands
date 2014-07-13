{-# LANGUAGE OverloadedStrings #-}
module Handler.Posts where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql
import Model.Post
import Text.Blaze.Html
import Util
import qualified View.Posts
import Web.Spock

index :: RequestedFormat () -> WebAction ()
index (HtmlRequested ()) =
    hostname >>= loadPosts >>= myBlaze "index" . View.Posts.index
index (JsonRequested ()) =
    hostname >>= loadPosts >>= json
index _ = show404

show :: T.Text -> RequestedFormat T.Text -> WebAction ()
show host (HtmlRequested slug) = do
    -- TYPE Host, Slug...
    post <- runSQL $ loadPost host slug
    case post of
        Just p  -> myBlaze "Post" . View.Posts.show $ entityVal p
        _       -> show404
show host (JsonRequested slug) = do
    post <- runSQL $ loadPost host slug
    case post of
        Just p  -> json $ entityVal p
        -- How about a JSON error?
        _       -> show404
show _ _ = show404

-- TODO Multiple Tags
tagged tag = undefined

loadPost :: T.Text -> T.Text -> SqlPersistM (Maybe (Entity Post))
loadPost host slug = selectFirst [PostSlug ==. slug, PostDomain ==. host] []

loadPosts :: T.Text -> WebAction [Post]
loadPosts host = fmap (map entityVal) $ runSQL $ selectList [PostDomain ==. host] []
