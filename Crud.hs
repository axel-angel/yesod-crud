{-# LANGUAGE OverloadedStrings #-}
module Crud where

import Prelude
import Model
import Yesod
--import Yesod.Form
import Data.Text
--import Text.Shakespeare.I18N
import Control.Applicative

{- EntityForm -}
class EntityForm a where
    aform :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
        Maybe a -> AForm m a

instance EntityForm Faq where
    aform e = Faq
        <$> areq toField "Name" (faqName <$> e)
        <*> areq toField "Content" (faqContent <$> e)
        <*> areq toField "Order" (faqOrder <$> e)

{- EntityFieldsForm -}
class EntityFieldsForm a where
    fieldValue :: EntityField a t -> a -> t

    -- FIXME: generalize to RenderMessage site msg
    fieldLabel :: EntityField a t -> Text

    afield :: (RenderMessage site FormMessage, MonadHandler m,
               HandlerSite m ~ site, ToField t) =>
        EntityField a t -> Maybe a -> AForm m t

instance EntityFieldsForm Faq where
    fieldValue FaqId = undefined -- FIXME
    fieldValue FaqName = faqName
    fieldValue FaqContent = faqContent
    fieldValue FaqOrder = faqOrder

    fieldLabel FaqId = ("Id" :: Text)
    fieldLabel FaqName = ("Name" :: Text)
    fieldLabel FaqContent = ("Content" :: Text)
    fieldLabel FaqOrder = ("Order" :: Text)

    afield field e = areq toField (fieldSettingsLabel $ fieldLabel field) (fieldValue field <$> e)

{- ToField -}
class ToField a where
    toField :: Monad m => RenderMessage (HandlerSite m) FormMessage =>
        Field m a

instance ToField Text where
    toField = textField

instance ToField Int where
    toField = intField
