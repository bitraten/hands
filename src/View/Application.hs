{-# LANGUAGE OverloadedStrings #-}
module View.Application where

import Prelude hiding (head, id)
import Model.User
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

html :: User -> Html -> Html -> Html
html user pageTitle content = docTypeHtml $ do
    head $ do 
        title pageTitle
        link ! href "/assets/application.css" ! rel "stylesheet"
    body $ do
        section ! id "main" $ content
