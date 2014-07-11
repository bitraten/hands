{-# LANGUAGE OverloadedStrings #-}
module View.Application where

import Prelude hiding (head, id)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

html :: Html -> Html -> Html
html pageTitle content = docTypeHtml $ do
    head $ do 
        title pageTitle
        link ! href "/assets/application.css" ! rel "stylesheet"
    body $ do
        section ! id "main" $ content
