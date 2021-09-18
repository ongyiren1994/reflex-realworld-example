{-# LANGUAGE FlexibleContexts, LambdaCase, MonoLocalBinds, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RecursiveDo, ScopedTypeVariables                                      #-}

module Frontend.Article where

import Control.Lens
import Reflex.Dom.Core hiding (Element)

import           Control.Applicative    (liftA2)
import           Control.Monad          (mfilter)
import           Control.Monad.Fix      (MonadFix)
import           Data.Default           (def)
import           Data.Foldable          (fold)
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            (Endo (Endo), appEndo)
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as TL
import           GHCJS.DOM.Document     (createElement)
import           GHCJS.DOM.Element      (setInnerHTML)
import           GHCJS.DOM.Types        (liftJSM)
import qualified Lucid                  as L
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, Routed, SetRoute, askRoute, routeLink, setRoute)
import qualified Text.MMark             as MMark


import qualified Common.Conduit.Api.Articles.Article       as Article
import qualified Common.Conduit.Api.Articles.Comment       as Comment
import qualified Common.Conduit.Api.Articles.CreateComment as CreateComment
import           Common.Conduit.Api.Namespace              (Namespace (..), unNamespace)
import qualified Common.Conduit.Api.Profiles.Profile       as Profile
import qualified Common.Conduit.Api.User.Account           as Account
import           Common.Route                              (DocumentSlug (..), FrontendRoute (..),
                                                            Username (..))
import           Frontend.ArticlePreview                   (profileImage, profileRoute)
import qualified Frontend.Conduit.Client                   as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                            (buttonClass, routeLinkClass, routeLinkDynClass,
                                                            showText, modifyFormAttrs)
import Reflex.Dom (Prerender)
import qualified Frontend.Conduit.Client as Client
import           Servant.Common.Req     (QParam (..))

article
  :: forall t m js s
  .  ( DomBuilder t m
     , Prerender js t m
     , Routed t DocumentSlug m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
article = elClass "div" "article-page" $ do
  -- We ask our route for the document slug and make the backend call on load
  slugDyn <- askRoute
  pbE <- getPostBuild
  tokDyn <- reviewFrontendState loggedInToken

  (successE,_,_) <- Client.getArticle tokDyn (pure . unDocumentSlug <$> slugDyn) $ pbE <> (void $ updated tokDyn)

  articleDyn <- holdDyn Nothing $ Just . unNamespace <$> successE

  elClass "div" "banner" $
    elClass "div" "container" $ do
      el "h1" $ dynText $ maybe "" Article.title <$> articleDyn
      -- We are a little clumsy with dealing with not having
      -- an article. We just disply a blank element while we
      -- dont have one. Should be better. :)
      void $ dyn $ maybe blank articleMeta <$> articleDyn
  elClass "div" "container page" $ do
    articleContent articleDyn
    el "hr" blank
    -- Removed for better looks
    -- elClass "div" "row article-actions" $
    --   void $ dyn $ maybe blank articleMeta <$> articleDyn
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-8 offset-md-2" $ do
        -- Do the comments UI below
        comments slugDyn

articleMeta
  :: ( DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , PostBuild t m
     , MonadHold t m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     , MonadHold t m
     , MonadFix m
     )
  -- The article is static, use articleDyn instead?
  => Article.Article
  -> m ()
articleMeta art = elClass "div" "article-meta" $ do
  let profile = Article.author art
  let authorRoute = FrontendRoute_Profile :/ (Username "foo", Nothing)
  routeLink authorRoute $ profileImage "" (constDyn . Profile.image $ profile)
  elClass "div" "info" $ do
    routeLinkClass "author" authorRoute $ text (Profile.username profile)
    elClass "span" "date" $ text (showText $ Article.createdAt art)
  actions profile
  where
    actions profile = mdo
      accountDyn <- reviewFrontendState loggedInAccount
      followE <- buttonClass "btn btn-sm btn-outline-secondary action-btn" (constDyn False) $ do
        elClass "i" "ion-plus-round" blank
        dynText $ fmap (\x -> if x then " Following " <> (Profile.username profile) else " Follow " <> (Profile.username profile)) followDyn
        text " ("
        -- TODO : Get this value
        elClass "span" "counter" $ text "0"
        text ")"
      text " "
      -- Abit dodgy, can be improved
      favE <- buttonClass "btn btn-sm btn-outline-primary action-btn" (constDyn False) $ do
        elClass "i" "ion-heart" blank
        dynText $ fmap (\x -> if x then " Favoring Post" else " Favorite Post" ) favDyn
        text " ("
        dynText $ fmap showText favCountDyn
        text ")"
      favCountDyn <- foldDyn ($) (Article.favoritesCount art :: Integer) (fmap (\x -> if x then (+ 1) else (+ (-1))) (updated favDyn))
      favDyn <- toggle (Article.favorited art :: Bool) favEE
      followDyn <- toggle (Profile.following profile :: Bool) followEE
      (favEE ,_ ,_) <- Client.favorite ((fmap . fmap) Account.token accountDyn) ( fmap QParamSome (Article.slug <$> constDyn art)) ( fmap QParamSome favDyn) favE
      (followEE ,_ ,_) <- Client.follow ((fmap . fmap) Account.token accountDyn) (fmap QParamSome (constDyn (Profile.username profile))) (fmap QParamSome followDyn) followE
      -- Can be done better here?
      editEE <- dyn $
        liftA2
          (\p -> maybe (pure never) editButton
            . mfilter ((Profile.username p ==). Account.username)
          ) (constDyn profile) accountDyn
      switchHold never editEE
      deleteEE <- dyn $
        liftA2
          (\p -> maybe (pure never) deleteButton
            . mfilter ((Profile.username p ==). Account.username)
          ) (constDyn profile) accountDyn
      switchHold never deleteEE
      pure ()
      where
        editButton _ = do
            editClickE <- elClass "button" "btn btn-outline-primary btn-sm" $ do
              (editElt ,_) <- elClass' "i" "ion-edit" blank
              pure $ domEvent Click editElt
            setRoute $
              (\_ ->
                FrontendRoute_Editor :/ Just (DocumentSlug $ Article.slug art)
              ) <$> editClickE
            pure (void editClickE)
        deleteButton account = do
            deleteClickE <- elClass "button" "btn btn-outline-primary btn-sm" $ do
              (trashElt,_) <- elClass' "i" "ion-trash-a" blank
              pure $ domEvent Click trashElt
            (deleteSuccessE,_,_) <- Client.deleteArticle
              (constDyn . Just . Account.token $ account)
              (Right . Article.slug <$> constDyn art)
              deleteClickE
            setRoute $
              (\_ ->
                FrontendRoute_Profile :/ (Username $ Account.username account, Nothing)
              ) <$> deleteSuccessE
            pure (void deleteSuccessE)


articleContent
  :: forall t m js
  .  ( DomBuilder t m
     , Prerender js t m
     )
  => Dynamic t (Maybe Article.Article)
  -> m ()
articleContent articleDyn = prerender_ (text "Rendering Document...") $ do
  let htmlDyn = (fromMaybe "" . fmap (markDownToHtml5 . Article.body)) <$> articleDyn
  elClass "div" "row article-content" $ do
    d <- askDocument
    -- We have to sample the initial value to set it on creation
    htmlT <- sample . current $ htmlDyn
    e <- liftJSM $ do
      -- This wont execute scripts, but will allow users to XSS attack through
      -- event handling javascript attributes in any raw HTML that is let
      -- through the markdown renderer. But this is the simplest demo that
      -- mostly works. See https://github.com/qfpl/reflex-dom-template for a
      -- potentially more robust solution (we could filter out js handler attrs
      -- with something like that).
      -- It's worth noting that the react demo app does exactly what this does:
      -- https://github.com/gothinkster/react-redux-realworld-example-app/blob/master/src/components/Article/index.js#L60
      e <- createElement d ("div" :: String)
      setInnerHTML e htmlT
      pure e
    -- And make sure we update the html when the article changes
    performEvent_ $ (liftJSM . setInnerHTML e) <$> updated htmlDyn
    -- Put out raw element into our DomBuilder
    placeRawElement e

markDownToHtml5 :: Text -> Text
markDownToHtml5 t =
  case MMark.parse "" t of
    Left _  -> ""
    Right r -> TL.toStrict . L.renderText . MMark.render $ r


comments
  :: forall t m s js
  .  ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , PostBuild t m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     , Prerender js t m
     , MonadFix m
     , MonadHold t m
     )
  => Dynamic t DocumentSlug
  -> m ()
comments slugDyn = mdo
  -- Load the comments when this widget is built
  pbE <- getPostBuild
  accountDyn <- reviewFrontendState loggedInAccount
  let tokenDyn = fmap Account.token <$> accountDyn
  (successE,_,_) <- Client.getComments
    tokenDyn
    (Right . unDocumentSlug <$> slugDyn)
    (leftmost [pbE, void $ updated slugDyn, void $ updated tokenDyn ])

  -- Turn it into a map so that we have IDs for each comment
  let loadedMapE   = Map.fromList . (fmap (\c -> (Comment.id c, c))) . unNamespace <$> successE

  -- Our state actually includes the AJAX load and future comment adds
  commentsMapDyn <- foldDyn appEndo Map.empty $ fold
    [ Endo . const <$> loadedMapE
    , (\newComment -> Endo $ Map.insert (Comment.id newComment) newComment) <$> newCommentE
    , ( foldMap (Endo . Map.delete) . Map.keys ) <$> deleteComment
    ]

  newEE <- dyn $ maybe (pure never) addCommentWidget <$> tokenDyn
  newCommentE <- switchHold never newEE
  -- This takes the Map Integer Comment and displays them all
  deleteComment :: Event t (Map.Map Integer ()) <- listViewWithKey commentsMapDyn $ \cId commentDyn -> do
    let profileDyn = Comment.author <$> commentDyn
    elClass "div" "card" $ do
      elClass "div" "card-block" $ do
        elClass "p" "card-text" . dynText $ Comment.body <$> commentDyn
      elClass "div" "card-footer" $ do
        let authorRouteDyn = profileRoute <$> profileDyn
        routeLinkDynClass (constDyn "comment-author") authorRouteDyn $
          profileImage "comment-author-img" (Profile.image <$> profileDyn)
        text " "
        routeLinkDynClass "comment-author" authorRouteDyn $ dynText $ Profile.username <$> profileDyn
        elClass "span" "date-posted" $ display $ Comment.createdAt <$> commentDyn
        deleteEE <- dyn $
          liftA2
            (\p -> maybe (pure never) (deleteButton cId)
              . mfilter (((Profile.username p) ==). Account.username)
            )
            profileDyn
            accountDyn
        switchHold never deleteEE
  pure ()
  where
    addCommentWidget token = do
      -- Make a form that will add a comment with the backend and return
      -- an event when they are successfully added.
      elClass "form" "card comment-form" $ mdo
        commentI <- elClass "div" "card-block" $ do
          let attrs = Map.fromList [("class","form-control"),("placeholder","Write a comment"),("rows","3")]
          modifyE <- modifyFormAttrs attrs submittingDyn (constDyn False)
          textAreaElement $ def
              & textAreaElementConfig_elementConfig.elementConfig_initialAttributes .~ attrs
              & textAreaElementConfig_setValue .~ ("" <$ submitSuccessE)
              & textAreaElementConfig_elementConfig.elementConfig_modifyAttributes .~ modifyE
        let createCommentDyn = Right . Namespace <$> CreateComment.CreateComment
              <$> commentI ^. to _textAreaElement_value
        postE <- elClass "div" "card-footer" $ do
          --  Comment nothing shall be allowed
          buttonClass "btn btn-sm btn-primary" (constDyn False) $ text "Post Comment"
        (submitSuccessE, _, submittingDyn) <- Client.createComment
          (constDyn . Just $ token)
          (Right . unDocumentSlug <$> slugDyn)
          createCommentDyn postE
        pure (unNamespace <$> submitSuccessE)
    deleteButton cId account = do
      deleteClickE <- elClass "span" "mod-options" $ do
        (trashElt,_) <- elClass' "i" "ion-trash-a" blank
        pure $ domEvent Click trashElt
      (deleteSuccessE,_,_) <- Client.deleteComment
        (constDyn . Just . Account.token $ account)
        (Right . unDocumentSlug <$> slugDyn)
        (constDyn . Right $ cId)
        deleteClickE
      pure (void deleteSuccessE)
