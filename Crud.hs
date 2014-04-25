{-# LANGUAGE OverloadedStrings #-}
module Crud where

import Prelude
import Model
import Yesod
--import Yesod.Form
import Data.Text (Text)
--import Text.Shakespeare.I18N
import Control.Applicative

{- EntityCrud -}
class EntityCrud a where
    render :: Monad m => FormRender m a
    render = renderDivs

    addWidget :: (RenderMessage (HandlerSite m) FormMessage, MonadResource m,
            MonadHandler m, EntityField a (KeyBackend bak a)) =>
        m (xml, Maybe FaqId)

instance EntityCrud Faq where
    addWidget = do
        --let renderedForm = renderDivs $ toAForm (Nothing :: Maybe Faq)
        _ <- runDB $ selectList ([]::[Filter Faq]) []
        ((result, formWidget), formEnctype) <- runFormPost $ renderDivs $ toAForm (Nothing :: Maybe Faq)
        let _ = result :: FormResult Faq
            _ = formEnctype :: Enctype

        {-
        fIdMay <- case result of
             FormSuccess f -> Just <$> (runDB $ insert f)
             _ -> return Nothing
         -}

        Just thisRoute <- getCurrentRoute
        let form = [whamlet|
            <form method=post action=@{thisRoute} enctype=#{formEnctype}>
              ^{formWidget}
              <button type=submit>Send it
        |]

        return (form, undefined)
        --return (form, fIdMay)

{- EntityForm -}
class EntityForm a where
    toAForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
        Maybe a -> AForm m a

instance EntityForm Faq where
    toAForm entityMay = Faq
        <$> fieldForm FaqName entityMay
        <*> fieldForm FaqContent entityMay
        <*> fieldForm FaqOrder entityMay

{- Utils -}
fieldForm :: (RenderMessage site FormMessage, MonadHandler m,
           HandlerSite m ~ site, ToField t, EntityFieldsForm a) =>
    EntityField a t -> Maybe a -> AForm m t
fieldForm field entityMay = areq toField
    (fieldSettingsLabel $ fieldLabel field)
    (fieldValue field <$> entityMay)

{- EntityFieldsForm -}
class EntityFieldsForm a where
    fieldValue :: EntityField a t -> a -> t

    -- FIXME: generalize to RenderMessage site msg
    fieldLabel :: EntityField a t -> Text

instance EntityFieldsForm Faq where
    fieldValue FaqId = undefined -- FIXME
    fieldValue FaqName = faqName
    fieldValue FaqContent = faqContent
    fieldValue FaqOrder = faqOrder

    -- to move into an user-definable instance
    fieldLabel FaqId = ("Id" :: Text)
    fieldLabel FaqName = ("Name" :: Text)
    fieldLabel FaqContent = ("Content" :: Text)
    fieldLabel FaqOrder = ("Order" :: Text)

{- ToField -}
class ToField a where
    toField :: Monad m => RenderMessage (HandlerSite m) FormMessage =>
        Field m a

instance ToField Text where
    toField = textField

instance ToField Int where
    toField = intField
