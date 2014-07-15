{-# LANGUAGE OverloadedStrings #-}
module View.Posts where

import Prelude hiding (show, div)
import Control.Monad (forM_)
import Model.Post
import Model.User
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

index :: User -> [Post] -> Html
index user posts = do
    forM_ posts $ do
        show user

show :: User -> Post -> Html
show user post = do
    article ! class_ "post" $ do
        header $ do
            div ! class_ "post-avatar" $ do
                a ! class_ "avatar" ! href "" $ do
                    img ! alt "" ! src ""
            ul ! class_ "post-information" $ do
                li ! class_ "post-author" $ do
                    a ! href "" $ "author"
                li ! class_ "post-time" $ do
                    a ! href "" $ "date"
                    a ! href "" $ "time"
        (div ! class_ "post-body") . preEscapedToHtml $ postBody_html post
        ul ! class_ "post-actions" $ do
            li ! class_ "post-edit-link" $ do
                a ! href "" $ "edit"
            li ! class_ "post-reply-link" $ do
                a ! href "" $ "reply"
