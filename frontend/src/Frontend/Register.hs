{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, RecursiveDo #-}
module Frontend.Register where

import Control.Lens
import Reflex.Dom.Core


import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)

import           Common.Conduit.Api.Namespace        (Namespace (Namespace), unNamespace)
import           Common.Conduit.Api.Users.Registrant (Registrant (Registrant))
import           Common.Route                        (FrontendRoute (..))
import qualified Frontend.Conduit.Client             as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                      (buttonClass, modifyFormAttrs, inputDynClass)
import Data.Foldable as Fold ( Foldable(foldr1) )
import Data.Text
import Control.Applicative (liftA2)
import Reflex.Dom (EventTag(SubmitTag))
import           Control.Monad.Fix      (MonadFix)

register
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty e) m
     , AsFrontendEvent e
     , HasFrontendState t s m
     , HasLoggedInAccount s
     , MonadHold t m
     , MonadFix m
     )
  => m ()
register = noUserWidget $ elClass "div" "auth-page" $ do
  elClass "div" "container-page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Sign up"
        elClass "p" "text-xs-center" $
          routeLink (FrontendRoute_Login :/ ()) $ text "Have an account?"
        elClass "ul" "error-messages" $
          blank
        el "form" $ mdo
          (usernameBlurE, usernameI) <- elClass "fieldset" "form-group" $ do
            let attrs = Map.fromList [ ("class","form-control form-control-lg") ,("placeholder","Your name")]
            modifyE <- modifyFormAttrs attrs submittingDyn isUsernameEmptyDyn
            inputDynClass attrs modifyE
          (emailBlurE, emailI) <- elClass "fieldset" "form-group" $ do
            let attrs = Map.fromList [ ("class","form-control form-control-lg"), ("placeholder","Email")]
            modifyE <- modifyFormAttrs attrs submittingDyn isEmailEmptyDyn
            inputDynClass attrs modifyE
          (passBlurE, passI) <- elClass "fieldset" "form-group" $ do
            let attrs = Map.fromList [ ("class","form-control form-control-lg"), ("placeholder","Password"), ("type","password")]
            modifyE <- modifyFormAttrs attrs submittingDyn isPassEmptyDyn
            inputDynClass attrs modifyE
          isEmailBlurDyn <- holdDyn False (True <$ emailBlurE)
          isPassBlurDyn <- holdDyn False (True <$ passBlurE)
          isUsernameBlurDyn <- holdDyn False (True <$ usernameBlurE)
          let isUsernameEmptyDyn = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (usernameI ^. to _inputElement_value), isUsernameBlurDyn]
              isEmailEmptyDyn  = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (emailI ^. to _inputElement_value), isEmailBlurDyn]
              isPassEmptyDyn  = Fold.foldr1 (liftA2 (&&)) [fmap ((== "") . strip) (passI ^. to _inputElement_value), isPassBlurDyn]
              isValidDyn = Fold.foldr1 (liftA2 (||)) [isEmailEmptyDyn, isPassEmptyDyn, isUsernameEmptyDyn]
          submitE <- buttonClass "btn btn-lg btn-primary pull-xs-right" isValidDyn $ text "Sign Up"
          let registrant = Registrant
                <$> usernameI ^. to _inputElement_value
                <*> emailI ^. to _inputElement_value
                <*> passI ^. to _inputElement_value
          (successE,_, submittingDyn) <- Client.register (pure . Namespace <$> registrant) submitE
          tellEvent (fmap (pure . (_LogIn #) . unNamespace) $ successE)
          pure ()
  pure ()
