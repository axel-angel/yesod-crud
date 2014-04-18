module Crud where

import Model
import Yesod.Form
import Text.Shakespeare.I18N
import Control.Applicative

{- EntityForm -}
class EntityForm a where
    aform :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
        Maybe a -> AForm m a

instance EntityForm Faq where
    aform e = Faq
        <$> areq field "Name" (faqName <$> e)
        <*> areq field "Content" (faqContent <$> e)
        <*> areq field "Order" (faqOrder <$> e)

{- EntityFieldsForm -}
class EntityFieldsForm a where
    fieldValue :: EntityField a t -> a -> t

    fieldLabel :: (RenderMessage site msg, MonadHandler m,
               HandlerSite m ~ site) =>
        EntityField a t -> msg

    afield :: (RenderMessage site FormMessage, MonadHandler m,
               HandlerSite m ~ site) =>
        EntityField a t -> Maybe a -> AForm m t

instance EntityFieldsForm Faq where
    fieldValue FaqId = undefined -- FIXME
    fieldValue FaqName = faqName
    fieldValue FaqContent = faqContent
    fieldValue FaqOrder = faqOrder

    fieldLabel FaqId = SomeMessage $ ToMessage "Id"
    fieldLabel FaqName = SomeMessage $ ToMessage "Name"
    fieldLabel FaqContent = SomeMessage $ ToMessage "Content"
    fieldLabel FaqOrder = SomeMessage $ ToMessage "Order"

    afield field e = areq field fieldLabel field (fieldValue field <$> e)

{- ToField -}
class ToField a where
    field :: Monad m => RenderMessage (HandlerSite m) FormMessage =>
        Field m a

instance ToField Text where
    field = textField

instance ToField Int where
    field = intField
