{-# LANGUAGE OverloadedStrings #-}
module View.Posts where

import Prelude hiding (div)
import Control.Monad (forM_)
import Data.Time.Format (formatTime)
import Model.Post
import Model.User
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes


index :: User -> [Post] -> Html
index user posts = do
    forM_ posts $ do
        View.Posts.show user

show :: User -> Post -> Html
show author post = do
    article ! class_ "post" $ do
        header $ do
            ul ! class_ "post-information" $ do
                li ! class_ "post-author" $ do
                    div ! class_ "post-avatar" $
                        a ! class_ "avatar" ! href uUrl $
                            img ! alt "" ! src ""
                    a ! href uUrl $ toHtml $ userDisplay_name author
                li ! class_ "post-time" $ do
                    a ! href "" $ date
                    preEscapedToHtml middot
                    a ! href pUrl $ time
        (div ! class_ "post-body") . preEscapedToHtml $ postBody_html post
        ul ! class_ "post-actions" $ do
            li ! class_ "post-edit-link" $ do
                a ! href "" $ "edit"
            li ! class_ "post-reply-link" $ do
                a ! href "" $ "reply"
    where   uUrl = toValue . Prelude.show $ userUrl author
            pUrl = toValue . Prelude.show $ postUrl post
            date = toHtml $ formatTime defaultTimeLocale "%b %d" $ postPublished_at post
            time = toHtml $ formatTime defaultTimeLocale "%H:%M" $ postPublished_at post
            middot = " &middot; " :: String
