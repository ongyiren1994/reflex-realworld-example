{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables                                                                     #-}
module Frontend.HomePage where

import Reflex.Dom.Core

import Control.Monad.Fix      (MonadFix)
import Data.Functor           (void)
import Obelisk.Route          (pattern (:/), R)
import Obelisk.Route.Frontend (RouteToUrl, SetRoute)
import Servant.Common.Req     (QParam (QNone))

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Route                         (FrontendRoute (..))
import           Frontend.ArticlePreview              (articlesPreview)
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (routeLinkClass)

homePage
  :: forall t m s js
  . ( PostBuild t m
     , DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     )
  => m ()
homePage = elClass "div" "home-page" $ do
  tokDyn <- reviewFrontendState loggedInToken
  pbE <- getPostBuild
  (loadArtsE,_,_) <- Client.listArticles
    tokDyn
    (constDyn QNone)
    (constDyn QNone)
    (constDyn [])
    (constDyn [])
    (constDyn [])
    (leftmost [pbE,void $ updated tokDyn])

  artsDyn <- holdDyn (Articles [] 0) loadArtsE
  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"
  elClass "div" "container page" $ elClass "div" "row" $ do
    elClass "div" "col-md-9" $ do
      elClass "div" "feed-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          elClass "li" "nav-item" $ routeLinkClass "nav-link disabled" (FrontendRoute_Home :/ ()) $ text "Your Feed"
          elClass "li" "nav-item" $ routeLinkClass "nav-link active" (FrontendRoute_Home :/ ()) $ text "Global Feed"
      articlesPreview artsDyn

    elClass "div" "col-md-3" $
      elClass "div" "sidebar" $ do
        el "p" $ text "Popular Tags"
        elClass "div" "tag-list" $ do
          tagPill "programming"
          tagPill "javascript"
          tagPill "emberjs"
          tagPill "angularjs"
          tagPill "react"
          tagPill "mean"
          tagPill "node"
          tagPill "rails"
  where
    tagPill t = routeLinkClass "tag-pill tag-default" (FrontendRoute_Home :/ ()) $ text t
