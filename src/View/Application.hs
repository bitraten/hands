{-# LANGUAGE OverloadedStrings #-}
module View.Application where

import Prelude hiding (head, id, div)
import Model.User
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

html :: User -> Html -> Html -> Html
html user pageTitle content = docTypeHtml $ do
    head $ do
        title pageTitle
        link ! href "/assets/application.css" ! rel "stylesheet"
    body $ do
        header ! class_ "with-flair" ! rolePrimary $ do
            div ! class_ "header-container" $ do
                a ! class_ "header-site-image" $ do
                   img ! src ""
                div ! class_ "header-site-info" $ do
                    div ! class_ "header-site-name" $ do
                        a ! href "/" $ toHtml $ userDisplay_name user
                    div ! class_ "header-site-domain" $ do
                        a ! href "/" $ toHtml $ userDomain user
        navigation
        section ! id "main" $ content
        footer ! rolePrimary $ do
            p $ do
                strong $ a ! href "/" $ toHtml $ userDisplay_name user
                " is running on "
                strong $ a ! href "https://github.com/bitraten/hands" $ "hands"
                ", a clone of #pants"

navigation :: Html
navigation = do
    nav ! rolePrimary $ ul $ do
            li ! class_ "active" $ a ! href "/" $ "Blog"
            li $ a ! href "/network" $ "Network"
            li $ a ! href "/following" $ "Following"
            li $ a ! href "/user/edit" $ "Settings"
            li $ a ! href "/login" $ i ! class_ "fa fa-sign-out" $ ""

rolePrimary = customAttribute "role" "primary"
