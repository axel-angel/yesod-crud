{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Home where

import Import
import Crud

derivePersistForm "Faq"

instance EntityForm Faq where
    toAForm entityMay = Faq
        <$> fieldForm FaqName entityMay
        <*> fieldForm FaqContent entityMay
        <*> fieldForm FaqOrder entityMay

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
handleHomeR :: Handler Html
handleHomeR = do
    let aForm = toAForm (Nothing :: Maybe Faq)
    ((fRes, widget), fEnc) <- runFormPost $ renderDivs aForm

    defaultLayout $ do
        $(widgetFile "homepage")
