{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RecursiveDo                                                  #-}
module Frontend.Profile where

import Reflex.Dom.Core

import Control.Monad.Fix      (MonadFix)
import Data.Bool              (bool)
import Data.Functor           (void)
import Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, askRoute)
import Servant.Common.Req     (QParam (QNone))
import           Servant.Common.Req     (QParam (..))

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Conduit.Api.Articles.Article
import           Common.Conduit.Api.Namespace         (Namespace(Namespace))
import qualified Common.Conduit.Api.Profiles.Profile  as Profile
import qualified Common.Conduit.Api.Profiles.Follow  as Follow
import           Common.Route                         (FrontendRoute (..), ProfileRoute (..), Username (..))
import           Frontend.ArticlePreview              (articlesPreview, profileImage)
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, routeLinkDynClass)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile usernameDyn = do
  pbE <- getPostBuild
  tokDyn <- reviewFrontendState loggedInToken
  elClass "div" "profile-page" $ do
  elClass "div" "user-info" $
    elClass "div" "container" $
      elClass "div" "row" $
        elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
          (loadSuccessE,_,_) <- Client.getProfile
            tokDyn
            (pure . unUsername <$> usernameDyn)
            (leftmost [pbE,void . updated $ usernameDyn])
          void $ widgetHold (text "Loading") $ ffor loadSuccessE $ \(Namespace acct) -> mdo
            profileImage "user-img" (constDyn $ Profile.image acct)
            el "h4" $ text $ Profile.username acct
            el "p" $ text $ Profile.bio acct
            followE <- buttonClass "btn btn-sm btn-outline-secondary action-btn" (constDyn False) $ do
              elClass "i" "ion-plus-round" blank
              dynText $ fmap (\x -> if x then " Following " <> (Profile.username acct) else " Follow " <> (Profile.username acct)) followDyn
            followDyn <- toggle (Profile.following acct :: Bool) followEE
            (followEE ,_ ,_) <- Client.follow tokDyn ((Right . Namespace) <$> (Follow.Follow <$> constDyn (Profile.username acct) <*> followDyn)) followE
            text " "
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        elClass "div" "articles-toggle" $ mdo
          rDyn <- elClass "ul" "nav nav-pills outline-active" $ do
            rDyn <- askRoute
            navItem Nothing rDyn $ text "My Articles"
            navItem (Just $ ProfileRoute_Favourites :/ ()) rDyn $ text "My Favourites"
            pure rDyn

          (loadArtsSuccessE,_,artsLoadingDyn) <- Client.listArticles
            tokDyn
            (constDyn QNone)
            (fmap (QParamSome . (*) 5) pageNum)
            (constDyn [])
            (pure . unUsername <$> usernameDyn)
            (constDyn [])
            (leftmost [pbE,void $ updated tokDyn, () <$ updated pageNum])

          artsDyn <- holdDyn (Articles [] 0) loadArtsSuccessE
          -- For Favorite tabs, basically we only show posts which are favorited by at least one person
          articlesPreview artsLoadingDyn (displayArticles <$> rDyn <*> artsDyn)

          let articleCount = fmap articlesCount (displayArticles <$> rDyn <*> artsDyn)
          let isLastPageDyn = fmap (< 5) articleCount
          let isFirstPageDyn = fmap (<= 0) pageNum
          pageNum <- elClass "div" "container" $ mdo
              decI <- buttonClass "btn btn-sm btn-outline-secondary action-btn" isFirstPageDyn $ do
                elClass "i" "ion-arrow-left-b" blank
              pageNum <- foldDyn ($) (0 :: Integer)  $ leftmost [(+) 1 <$ incI, (+ (-1)) <$ decI]
              el "span" $ display $ fmap (1 +) pageNum
              incI <- buttonClass "btn btn-sm btn-outline-secondary action-btn" isLastPageDyn $ do
                elClass "i" "ion-arrow-right-b" blank
              pure pageNum
          pure ()
  where
    navItem sr rDyn = elClass "li" "nav-item" . routeLinkDynClass
       (("nav-link " <>) . bool "" " active" . (== sr) <$> rDyn)
      ((\u -> FrontendRoute_Profile :/ (u,sr)) <$> usernameDyn)
    -- Can be written nicer here?
    -- Why must put type signature?
    displayArticles :: Maybe (R ProfileRoute) -> Articles -> Articles
    displayArticles sr = case sr of
                            Nothing -> (\x -> x)
                            Just (ProfileRoute_Favourites :/ ()) -> onlyFavoritePost
    onlyFavoritePost art = do
      let articles' = filter favorited (articles art)
      Articles  { articles = articles' , articlesCount = toInteger (length articles')}