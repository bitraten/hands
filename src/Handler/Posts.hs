{-# LANGUAGE OverloadedStrings #-}
module Handler.Posts where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql
import Model.Post
import Model.User
import Text.Blaze.Html
import Util
import qualified View.Posts
import Web.Spock

index :: RequestedFormat () -> User -> WebAction ()
index (HtmlRequested ()) user =
    loadPosts user >>= myBlaze "index" . View.Posts.index
index (JsonRequested ()) user =
    loadPosts user >>= json
index _ _ = show404

show :: RequestedFormat T.Text -> User -> WebAction ()
show (HtmlRequested slug) user = do
    -- TYPE Host, Slug...
    post <- runSQL $ loadPost user slug
    case post of
        Just p  -> myBlaze "Post" . View.Posts.show $ entityVal p
        _       -> show404
show (JsonRequested slug) user = do
    post <- runSQL $ loadPost user slug
    case post of
        Just p  -> json $ entityVal p
        -- How about a JSON error?
        _       -> show404
show _ _ = show404

-- TODO Multiple Tags
tagged tag = undefined

loadPost :: User -> T.Text -> SqlPersistM (Maybe (Entity Post))
loadPost user slug = selectFirst[PostSlug ==. slug,
                                 PostDomain ==. userDomain user] []

loadPosts :: User -> WebAction [Post]
loadPosts user = fmap (map entityVal) $ runSQL $
                    selectList [PostDomain ==. userDomain user] []
