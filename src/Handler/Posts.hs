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
    loadPosts user >>= myBlaze user "index" . View.Posts.index user
index (JsonRequested ()) user =
    loadPosts user >>= json
index _ user = show404 user

show :: RequestedFormat T.Text -> User -> WebAction ()
show (HtmlRequested slug) user =
    -- TYPE Slug…
    runSQL (loadPost user slug) >>=
    maybe (show404 user) (showFound)
    where showFound (Entity _ post) = do
            -- TODO Error handling
            Just author <- loadUser (postDomain $ post)
            myBlaze user "Post" $ View.Posts.show author post

show (JsonRequested slug) user =
    runSQL (loadPost user slug) >>=
        --How about a JSON error?
    maybe (show404 user) (json . entityVal)
show _ user = show404 user

-- TODO Multiple Tags
tagged tag = undefined

loadPost :: User -> T.Text -> SqlPersistM (Maybe (Entity Post))
loadPost user slug = selectFirst[PostSlug ==. slug,
                                 PostDomain ==. userDomain user] []

loadPosts :: User -> WebAction [Post]
loadPosts user = fmap (map entityVal) $ runSQL $
                    selectList [PostDomain ==. userDomain user] []
