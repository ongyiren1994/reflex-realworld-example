{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables                                                                     #-}
module Frontend.Editor where

import Control.Lens
import Reflex.Dom.Core

import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Obelisk.Route.Frontend (pattern (:/), R, Routed, SetRoute, setRoute, askRoute)

import qualified Common.Conduit.Api.Articles.Article    as Article
import           Common.Conduit.Api.Articles.Attributes (ArticleAttributes (..), CreateArticle, UpdateArticle)
import           Common.Conduit.Api.Namespace           (Namespace (Namespace), unNamespace)
import qualified Common.Conduit.Api.User.Account        as Account
import           Common.Route                           (DocumentSlug (..), FrontendRoute (..))
import qualified Frontend.Conduit.Client                as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                         (buttonClass)
import           Control.Monad.Fix      (MonadFix)
import           Data.Functor           (void)

editor
  :: forall t m js s
  . ( DomBuilder t m
     , PostBuild t m
     , Routed t (Maybe DocumentSlug) m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     , MonadFix m
     , MonadHold t m
     )
  => m ()
editor = userWidget $ \acct -> elClass "div" "editor-page" $ do
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        el "form" $
          el "fieldset" $ do
              maybeSlugDyn <- askRoute
              void $ dyn (maybe (editorNone acct) (editorJust acct) <$> maybeSlugDyn)

editorNone :: forall t m js s
  . ( DomBuilder t m
     , PostBuild t m
     , Routed t (Maybe DocumentSlug) m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     , MonadFix m
     , MonadHold t m
     )
  => Account.Account -> m ()
editorNone acct = do
  titleI <- elClass "fieldset" "form-group" $
    inputElement $ def
      & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
        [("class","form-control")
        ,("placeholder","Article Title")
        ])
  descI <- elClass "fieldset" "form-group" $
    inputElement $ def
      & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
        [("class","form-control")
        ,("placeholder","What's this article about?")
        ])
  bodyI <- elClass "fieldset" "form-group" $
    textAreaElement $ def
      & textAreaElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
        [("class","form-control")
        ,("placeholder","Write your article (in markdown)")
        ,("rows","8")
        ])
  publishE <- buttonClass "btn btn-lg btn-primary pull-xs-right" (constDyn False) $ text "Publish Article"
  let createArticle :: Dynamic t CreateArticle = ArticleAttributes
        <$> titleI ^. to _inputElement_value
        <*> descI  ^. to _inputElement_value
        <*> bodyI  ^. to _textAreaElement_value
        <*> constDyn Set.empty
  (successE,_,_) <- Client.createArticle
    (constDyn . Just $ Account.token acct)
    (pure . Namespace <$> createArticle)
    publishE

  setRoute $
    (\a -> FrontendRoute_Article :/ (DocumentSlug (Article.slug a)))
    . unNamespace
    <$> successE
  pure ()


editorJust :: forall t m js s
  . ( DomBuilder t m
     , PostBuild t m
     , Routed t (Maybe DocumentSlug) m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     , MonadFix m
     , MonadHold t m
     )
  => Account.Account -> DocumentSlug -> m ()
editorJust acct slug = do
  let tokenDyn = constDyn . pure . Account.token $ acct
  pbE <- getPostBuild
  (loadSuccessE,_,_) <- Client.getArticle tokenDyn (constDyn . Right $ unDocumentSlug slug) pbE
  let loadSlugE = unNamespace <$> loadSuccessE
  titleI <- elClass "fieldset" "form-group" $
    inputElement $ def
      & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
        [("class","form-control")
        ])
      & inputElementConfig_setValue .~ ( Article.title <$> loadSlugE)
  descI <- elClass "fieldset" "form-group" $
    inputElement $ def
      & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
        [("class","form-control")
        ])
      & inputElementConfig_setValue .~ ( Article.description  <$> loadSlugE)
  bodyI <- elClass "fieldset" "form-group" $
    textAreaElement $ def
      & textAreaElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
        [("class","form-control")
        ,("rows","8")
        ])
      & textAreaElementConfig_setValue .~ ( Article.body <$> loadSlugE)
  publishE <- buttonClass "btn btn-lg btn-primary pull-xs-right" (constDyn False) $ text "Update Article"
  let updateArticle :: Dynamic t UpdateArticle = ArticleAttributes
        <$> (Just <$> titleI ^. to _inputElement_value)
        <*> (Just <$> descI  ^. to _inputElement_value)
        <*> (Just <$> bodyI  ^. to _textAreaElement_value)
        <*> (Just <$> constDyn Set.empty)
  (successE,_,_) <- Client.updateArticle
    (constDyn . Just $ Account.token acct)
    (constDyn . Right $ unDocumentSlug slug )
    (pure . Namespace <$> updateArticle)
    publishE

  setRoute $
    (\a -> FrontendRoute_Article :/ (DocumentSlug (Article.slug a)))
    . unNamespace
    <$> successE
  pure ()
