module Foundation
    ( Hermes (..)
    , Route (..)
    , HermesMessage (..)
    , resourcesHermes
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , maybeAdmin
    , module Settings.StaticFiles
    , module Settings
    , module Model
    , module Yesod.ReCAPTCHA
    , Year
    , Month
    , Day
    , Slug
    ) where

import Prelude
import Yesod
import Yesod.ReCAPTCHA
import Yesod.Static
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Yesod.Form.Nic (YesodNic)
import Yesod.Form.Jquery
import Network.HTTP.Conduit (Manager)
#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Store
import Data.Text (pack, unpack, Text)
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Hermes = Hermes
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "Hermes" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype HermesRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Hermes = HermesRoute
-- * Creates the value resourcesHermes which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Hermes. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the HermesRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Hermes" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Hermes Hermes (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Hermes where
    approot = ApprootMaster $ appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized CreatePostR _ = isAdmin
    isAuthorized (EditPostR _) _ = isAdmin
    isAuthorized (DeletePostR _) _ = isAdmin
    isAuthorized CreateSPageR _ = isAdmin
    isAuthorized (EditSPageR _) _ = isAdmin
    isAuthorized (DeleteSPageR _) _ = isAdmin
    isAuthorized AdminR _ = isAdmin
    isAuthorized (DeleteCommentR _) _ = isAdmin
    isAuthorized (DeleteTagR _) _ = isAdmin

    isAuthorized _ _ = return Authorized

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

maybeAdmin :: GHandler s Hermes Bool
maybeAdmin = do
    auth <- maybeAuth
    case auth of
        Just mauth -> case (userIdent $ entityVal mauth) of
            "kostas868@gmail.com" -> return True
            _ -> return False
        Nothing -> return False

isAdmin :: GHandler s Hermes AuthResult
isAdmin = do
    auth <- requireAuth
    return $ case (userIdent $ entityVal auth) of
        "kostas868@gmail.com" -> Authorized
        _ -> Unauthorized "Nope."

-- to use nic wysiwyg
instance YesodNic Hermes
instance YesodJquery Hermes

-- How to run database actions.
instance YesodPersist Hermes where
    type YesodPersistBackend Hermes = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth Hermes where
    type AuthId Hermes = UserId

    -- Where to send a user after successful login
    loginDest _ = PostsR
    -- Where to send a user after logout
    logoutDest _ = PostsR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId, authGoogleEmail]

    authHttpManager = httpManager

instance YesodReCAPTCHA Hermes where
    recaptchaPublicKey = return "6Ld5Yc4SAAAAABWgA06zSg003Oia1tQ6MWxyPek5"
    recaptchaPrivateKey = return "6Ld5Yc4SAAAAAJVeFr6_3DNXA7OhMWXfjBOgWGIF"

-- Sends off your mail. Requires sendmail in production!
deliver :: Hermes -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Hermes FormMessage where
    renderMessage _ _ = defaultFormMessage



newtype Year = Year Int
    deriving (Show, Eq, Read)

instance PathPiece Year where
    toPathPiece (Year n) = pack $ show n
    fromPathPiece s =
        case reads $ unpack s of
            (i, _):_
                | i < 2000 -> Nothing
                -- yes, this blog will last for a thousand years!!!
                | i > 3000 -> Nothing
                | otherwise -> Just $ Year i
            [] -> Nothing

newtype Month = Month Int
    deriving (Show, Eq, Read)

instance PathPiece Month where
    toPathPiece (Month n) = pack $ show n
    fromPathPiece s =
        case reads $ unpack s of
            (i, _):_
                | i < 1 -> Nothing
                | i > 12 -> Nothing
                | otherwise -> Just $ Month i
            [] -> Nothing

newtype Day = Day Int
    deriving (Show, Eq, Read)

instance PathPiece Day where
    toPathPiece (Day n) = pack $ show n
    fromPathPiece s =
        case reads $ unpack s of
            (i, _):_
                | i < 1 -> Nothing
                | i > 31 -> Nothing
                | otherwise -> Just $ Day i
            [] -> Nothing

newtype Slug = Slug Text
    deriving (Show, Eq, Read)

instance PathPiece Slug where
    toPathPiece (Slug t) = t
    fromPathPiece s =
        case unpack s of
            (_:_) -> Just $ Slug s
            _ -> Nothing








