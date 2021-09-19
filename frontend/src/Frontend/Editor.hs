{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables, RecursiveDo                                                                     #-}
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
import           Frontend.Utils                         (buttonClass, modifyFormAttrs, inputDynClass, inputEDynClass, textAreaDynClass, textAreaEDynClass)
import           Control.Monad.Fix      (MonadFix)
import           Data.Functor           (void)
import Data.Text
import Control.Applicative (liftA2)
import Data.Foldable as Fold
import           GHCJS.DOM.Document     (createElement)
import           GHCJS.DOM.Element      (setInnerHTML)
import           GHCJS.DOM.Types        (liftJSM)
import           Data.Maybe             (fromMaybe)
import Frontend.Article (markDownToHtml5)

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
editorNone acct = mdo
  (titleBlurE,titleI) <- elClass "fieldset" "form-group" $ do
    let attrs = Map.fromList [("class","form-control"),("placeholder","Article Title")]
    modifyE <- modifyFormAttrs attrs submittingDyn isTitleEmptyDyn
    inputDynClass attrs modifyE
  (descBlurE, descI) <- elClass "fieldset" "form-group" $ do
    let attrs = Map.fromList [("class","form-control"),("placeholder","What's this article about?")]
    modifyE <- modifyFormAttrs attrs submittingDyn isDescEmptyDyn
    inputDynClass attrs modifyE
  (bodyBlurE, bodyI) <- elClass "fieldset" "form-group" $ do
    let attrs = Map.fromList [("class","form-control") ,("placeholder","Write your article (in markdown)") ,("rows","8")]
    modifyE <- modifyFormAttrs attrs submittingDyn isBodyEmptyDyn
    textAreaDynClass attrs modifyE
  bodyPreview (bodyI ^. to _textAreaElement_value)
  isTitleBlurDyn <- holdDyn False (True <$ titleBlurE)
  isDescBlurDyn <- holdDyn False (True <$ descBlurE)
  isBodyBlurDyn <- holdDyn False (True <$ bodyBlurE)
  let isTitleEmptyDyn :: Dynamic t Bool = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (titleI ^. to _inputElement_value), isTitleBlurDyn]
      isDescEmptyDyn :: Dynamic t Bool = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (descI ^. to _inputElement_value), isDescBlurDyn]
      isBodyEmptyDyn :: Dynamic t Bool = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (bodyI ^. to _textAreaElement_value), isBodyBlurDyn]
      isValidDyn = Fold.foldr1 (liftA2 (||)) [isBodyEmptyDyn, isDescEmptyDyn, isTitleEmptyDyn]
  publishE <- buttonClass "btn btn-lg btn-primary pull-xs-right" isValidDyn $ text "Publish Article"
  let createArticle :: Dynamic t CreateArticle = ArticleAttributes
        <$> titleI ^. to _inputElement_value
        <*> descI  ^. to _inputElement_value
        <*> bodyI  ^. to _textAreaElement_value
        <*> constDyn Set.empty
  (successE, _, submittingDyn) <- Client.createArticle
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
editorJust acct slug = mdo
  let tokenDyn = constDyn . pure . Account.token $ acct
  pbE <- getPostBuild
  (loadSuccessE,_,_) <- Client.getArticle tokenDyn (constDyn . Right $ unDocumentSlug slug) pbE
  let loadSlugE = unNamespace <$> loadSuccessE
  (titleBlurE, titleI) <- elClass "fieldset" "form-group" $ do
    let attrs = Map.fromList [("class","form-control")]
    modifyE <- modifyFormAttrs attrs submittingDyn isTitleEmptyDyn
    inputEDynClass attrs modifyE ( Article.title <$> loadSlugE)
  (descBlurE, descI) <- elClass "fieldset" "form-group" $ do
    let attrs = Map.fromList [("class","form-control")]
    modifyE <- modifyFormAttrs attrs submittingDyn isDescEmptyDyn
    inputEDynClass attrs modifyE ( Article.description  <$> loadSlugE)
  (bodyBlurE, bodyI) <- elClass "fieldset" "form-group" $ do
    let attrs = Map.fromList [("class","form-control") ,("rows","8")]
    modifyE <- modifyFormAttrs attrs submittingDyn isBodyEmptyDyn
    textAreaEDynClass attrs modifyE (Article.body <$> loadSlugE)
  bodyPreview (bodyI ^. to _textAreaElement_value)
  isTitleBlurDyn <- holdDyn False (True <$ titleBlurE)
  isDescBlurDyn <- holdDyn False (True <$ descBlurE)
  isBodyBlurDyn <- holdDyn False (True <$ bodyBlurE)
  let isTitleEmptyDyn :: Dynamic t Bool = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (titleI ^. to _inputElement_value), isTitleBlurDyn]
      isDescEmptyDyn :: Dynamic t Bool = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (descI ^. to _inputElement_value), isDescBlurDyn]
      isBodyEmptyDyn :: Dynamic t Bool = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (bodyI ^. to _textAreaElement_value), isBodyBlurDyn]
      isValidDyn = Fold.foldr1 (liftA2 (||)) [isBodyEmptyDyn, isDescEmptyDyn, isTitleEmptyDyn]
  publishE <- buttonClass "btn btn-lg btn-primary pull-xs-right" isValidDyn $ text "Update Article"
  let updateArticle :: Dynamic t UpdateArticle = ArticleAttributes
        <$> (Just <$> titleI ^. to _inputElement_value)
        <*> (Just <$> descI  ^. to _inputElement_value)
        <*> (Just <$> bodyI  ^. to _textAreaElement_value)
        <*> (Just <$> constDyn Set.empty)
  (successE, _, submittingDyn) <- Client.updateArticle
    (constDyn . Just $ Account.token acct)
    (constDyn . Right $ unDocumentSlug slug )
    (pure . Namespace <$> updateArticle)
    publishE

  setRoute $
    (\a -> FrontendRoute_Article :/ (DocumentSlug (Article.slug a)))
    . unNamespace
    <$> successE
  pure ()

bodyPreview
  :: forall t m js
  .  ( DomBuilder t m
     , Prerender js t m
     )
  => Dynamic t Text
  -> m ()
bodyPreview articleBody = prerender_ (text "Please type something...") $ do
  let htmlDyn = (fromMaybe "" . fmap (markDownToHtml5) <$> (Just <$> articleBody))
  -- UI to be improved
  elClass "h4" "" $ text "Preview"
  elClass "fieldset" "from-group" $ do
    d <- askDocument
    htmlT <- sample . current $ htmlDyn
    e <- liftJSM $ do
      e <- createElement d ("div" :: String)
      setInnerHTML e htmlT
      pure e
    performEvent_ $ (liftJSM . setInnerHTML e) <$> updated htmlDyn
    placeRawElement e