{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TemplateHaskell                                       #-}
module Frontend.HomePage where

import Control.Lens    hiding (element)
import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Functor           (void)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NEL
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import           Obelisk.Route          (R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute)
import           Servant.Common.Req     (QParam (..))

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Conduit.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Route                         (FrontendRoute (..))
import           Frontend.ArticlePreview              (articlesPreview)
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, buttonDynClass)

data HomePageSelected = GlobalSelected | FeedSelected | TagSelected Text deriving Show
makePrisms ''HomePageSelected

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
homePage = elClass "div" "home-page" $ mdo
  tokDyn <- reviewFrontendState loggedInToken
  pbE <- getPostBuild

  selectedDyn <- holdDyn GlobalSelected $ leftmost
    [ maybe GlobalSelected (const FeedSelected) <$> current tokDyn <@ pbE
    , maybe GlobalSelected (const FeedSelected) <$> updated tokDyn
    , NEL.head <$> newSelectedE
    ]

  res <- dyn $ ffor selectedDyn $ \s -> do
    newSelection <- getPostBuild
    case s of
      FeedSelected -> Client.feed
        tokDyn
        (constDyn QNone)
        (fmap (QParamSome . (*) 20) pageNum)
        newSelection

      GlobalSelected -> Client.listArticles
        tokDyn
        (constDyn QNone)
        (fmap (QParamSome . (*) 20) pageNum)
        (constDyn [])
        (constDyn [])
        (constDyn [])
        newSelection

      TagSelected t -> Client.listArticles
        tokDyn
        (constDyn QNone)
        (fmap (QParamSome . (*) 20) pageNum)
        (constDyn [t])
        (constDyn [])
        (constDyn [])
        newSelection

  (loadArtsE,_,artsLoadingDyn) <- Client.switchHoldThroughClientRes res
  (loadTagsE,_,_) <- Client.allTags (leftmost [pbE,void $ updated tokDyn])

  artsDyn <- holdDyn (Articles [] 0) loadArtsE
  tagsDyn <- holdDyn (Namespace []) loadTagsE

  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"
  (_, newSelectedE) <- runEventWriterT . elClass "div" "container page" . elClass "div" "row" $ do
    elClass "div" "col-md-9" $ do
      elClass "div" "feed-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          feedSelectEDyn <- dyn $ ffor tokDyn $ maybe (pure never) $ \_ -> do
            let feedClassDyn = ("nav-link" <>) . (^._FeedSelected.to (const " active")) <$> selectedDyn
            elClass "li" "nav-item" $ buttonDynClass feedClassDyn (constDyn False) $ text "Your Feed"
          feedSelectE''' <- switchHold never feedSelectEDyn
          tellEvent $ (FeedSelected :| []) <$ feedSelectE'''

          let homeClassDyn = ("nav-link" <>) . (^._GlobalSelected.to (const " active")) <$> selectedDyn
          globalSelectE''' <- elClass "li" "nav-item" $ buttonDynClass homeClassDyn (constDyn False) $ text "Global Feed"
          void . dyn . ffor selectedDyn $ \case
            TagSelected t -> elClass "li" "nav-item" $ buttonClass "nav-link active" (constDyn False) $ text $ "#" <> t
            _             -> pure never
          tellEvent $ (GlobalSelected :| []) <$ globalSelectE'''
          -- Why this works?
          void . dyn . ffor selectedDyn $ \case
            selected -> tellEvent $ (selected :| []) <$ updated pageNum

      articlesPreview artsLoadingDyn artsDyn

    elClass "div" "col-md-3" $
      elClass "div" "sidebar" $ do
        el "p" $ text "Popular Tags"
        elClass "div" "tag-list" $ do
          void $ simpleList (unNamespace <$> tagsDyn) tagPill

  let articleCount = fmap articlesCount artsDyn
  let isLastPageDyn = fmap (< 20) articleCount
  let isFirstPageDyn = fmap (<= 0) pageNum
  pageNum <- elClass "div" "container" $ mdo
      decI <- buttonClass "btn btn-sm btn-outline-secondary action-btn" isFirstPageDyn $ do
        elClass "i" "ion-arrow-left-b" blank
      -- UI to be improved.Create an event which only triggers (to reset to first page) when not first page and have 0 articleCount
      pageNum <- foldDyn ($) (0 :: Integer)  $ leftmost [(+) 1 <$ incI, (+ (-1)) <$ decI]
      el "span" $ display $ fmap (1 +) pageNum
      incI <- buttonClass "btn btn-sm btn-outline-secondary action-btn" isLastPageDyn $ do
        elClass "i" "ion-arrow-right-b" blank
      pure pageNum

  pure ()

  where
    tagPill tDyn = do
      let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
            & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
            & elementConfig_initialAttributes .~ ("class" =: "tag-pill tag-default" <> "href" =: "")
      (e, _) <- element "a" cfg $ dynText tDyn

      -- We'll gloss over this for now. But you can read this as:
      -- When the button is clicked, tag the event with the current value of the tDyn text.
      -- And then wrap it up in a Non empty list of HomeSelectedEvents (a list because EventWriter needs a semigroup)
      tellEvent $ pure . TagSelected <$> current tDyn <@ domEvent Click e
