{-# LANGUAGE CPP #-}
-- | This module is designed to work similarly to the Network.Browser module in the HTTP package.
-- The idea is that there are two new types defined: 'BrowserState' and 'BrowserAction'. The
-- purpose of this module is to make it easy to describe a browsing session, including navigating
-- to multiple pages, and have things like cookie jar updates work as expected as you browse
-- around.
--
-- BrowserAction is a monad that handles all your browser-related activities. This monad is
-- actually implemented as a specialization of the State monad, over the BrowserState type. The
-- BrowserState type has various bits of information that a web browser keeps, such as a current
-- cookie jar, the number of times to retry a request on failure, HTTP proxy information, etc. In
-- the BrowserAction monad, there is one BrowserState at any given time, and you can modify it by
-- using the convenience functions in this module.
--
-- A special kind of modification of the current browser state is the action of making a HTTP
-- request. This will do the request according to the params in the current BrowserState, as well
-- as modifying the current state with, for example, an updated cookie jar.
--
-- To use this module, you would bind together a series of BrowserActions (This simulates the user
-- clicking on links or using a settings dialogue etc.) to describe your browsing session. When
-- you've described your session, you call 'browse' on your top-level BrowserAction to actually
-- convert your actions into the ResourceT IO monad.
--
-- Here is an example program:
--
-- > import qualified Data.ByteString as B
-- > import qualified Data.ByteString.Lazy as LB
-- > import qualified Data.ByteString.UTF8 as UB
-- > import           Data.Conduit
-- > import           Network.HTTP.Conduit
-- > import           Network.HTTP.Conduit.Browser
-- > 
-- > -- The web request to log in to a service
-- > req1 :: IO (Request (ResourceT IO))
-- > req1 = do
-- >   req <- parseUrl "http://www.myurl.com/login.php"
-- >   return $ urlEncodedBody [ (UB.fromString "name", UB.fromString "litherum")
-- >                           , (UB.fromString "pass", UB.fromString "S33kRe7")
-- >                           ] req
-- > 
-- > -- Once authenticated, run this request
-- > req2 :: IO (Request m')
-- > req2 = parseUrl "http://www.myurl.com/main.php"
-- > 
-- > -- Bind two BrowserActions together
-- > action :: Request (ResourceT IO) -> Request (ResourceT IO) -> BrowserAction (Response LB.ByteString)
-- > action r1 r2 = do
-- >   _ <- makeRequestLbs r1
-- >   makeRequestLbs r2
-- > 
-- > main :: IO ()
-- > main = do
-- >   man <- newManager def
-- >   r1 <- req1
-- >   r2 <- req2
-- >   out <- runResourceT $ browse man $ action r1 r2
-- >   putStrLn $ UB.toString $ B.concat $ LB.toChunks $ responseBody out

module Network.HTTP.Conduit.Browser
    ( BrowserState
    , BrowserAction
    , browse
    , makeRequest
    , makeRequestLbs
    , defaultState
    , getBrowserState
    , setBrowserState
    , withBrowserState
    , getMaxRedirects
    , setMaxRedirects
    , withMaxRedirects
    , getMaxRetryCount
    , setMaxRetryCount
    , withMaxRetryCount
    , getTimeout
    , setTimeout
    , withTimeout
    , getAuthorities
    , setAuthorities
    , withAuthorities
    , getCookieFilter
    , setCookieFilter
    , withCookieFilter
    , getCookieJar
    , setCookieJar
    , withCookieJar
    , getCurrentProxy
    , setCurrentProxy
    , withCurrentProxy
    , getCurrentSocksProxy
    , setCurrentSocksProxy
    , withCurrentSocksProxy
    , getOverrideHeaders
    , setOverrideHeaders
    , withOverrideHeaders
    , insertOverrideHeader
    , deleteOverrideHeader
    , withOverrideHeader
    , getUserAgent
    , setUserAgent
    , withUserAgent
    , getCheckStatus
    , setCheckStatus
    , withCheckStatus
    , getDebug
    , setDebug
    , withDebug
    , getManager
    , setManager
    )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Control.Monad.State
import Control.Exception
import qualified Control.Exception.Lifted as LE
import Data.Conduit
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as HT
import Network.Socks5 (SocksConf)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.CaseInsensitive (mk)
import Data.ByteString.UTF8 (fromString)
import Data.List (partition)
import Web.Cookie (parseSetCookie)
import Data.Default (def)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map

import Network.HTTP.Conduit.Cookies hiding (updateCookieJar)
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Response
import Network.HTTP.Conduit.Manager
import qualified Network.HTTP.Conduit as HC

data BrowserState = BrowserState
  { maxRedirects        :: Maybe Int
  , maxRetryCount       :: Int
  , timeout             :: Maybe Int
  , authorities         :: Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)
  , cookieFilter        :: Request (ResourceT IO) -> Cookie -> IO Bool
  , cookieJar           :: CookieJar
  , currentProxy        :: Maybe Proxy
  , currentSocksProxy   :: Maybe SocksConf
  , overrideHeaders     :: Map.Map HT.HeaderName BS.ByteString
  , browserCheckStatus  :: Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException)
  , browserDebug        :: Maybe Bool
  , manager             :: Manager
  } 

defaultState :: Manager -> BrowserState
defaultState m = BrowserState { maxRedirects = Nothing
                              , maxRetryCount = 1
                              , timeout = Nothing
                              , authorities = \ _ -> Nothing
                              , cookieFilter = \ _ _ -> return True
                              , cookieJar = def
                              , currentProxy = Nothing
                              , currentSocksProxy = Nothing
                              , overrideHeaders = Map.singleton HT.hUserAgent (fromString "http-conduit")
                              , browserCheckStatus = Nothing
                              , browserDebug = Nothing
                              , manager = m
                              }

type BrowserAction = StateT BrowserState (ResourceT IO)

-- | Do the browser action with the given manager
browse :: Manager -> BrowserAction a -> ResourceT IO a
browse m act = evalStateT act (defaultState m)

-- | Make a request, using all the state in the current BrowserState
makeRequest :: Request (ResourceT IO) -> BrowserAction (Response (ResumableSource (ResourceT IO) BS.ByteString))
makeRequest request = do
  BrowserState
    { maxRetryCount = max_retry_count
    , maxRedirects = max_redirects
    , timeout = time_out
    , currentProxy  = current_proxy
    , currentSocksProxy = current_socks_proxy
    , overrideHeaders = override_headers
    , browserCheckStatus = current_check_status
    , browserDebug = current_debug
    } <- get
  retryHelper (applyOverrideHeaders override_headers $
    request { redirectCount = 0
            , proxy = maybe (proxy request) Just current_proxy
            , socksProxy = maybe (socksProxy request) Just current_socks_proxy
            , checkStatus = \ _ _ -> Nothing
            , responseTimeout = maybe (responseTimeout request) Just time_out
            , debug = fromMaybe (debug request) current_debug
            }) max_retry_count
               (fromMaybe (redirectCount request) max_redirects)
               (fromMaybe (checkStatus request) current_check_status)
               Nothing
  where retryHelper request' retry_count max_redirects check_status e
          | retry_count == 0 = case e of
            Just e' -> throw e'
            Nothing -> throw TooManyRetries
          | otherwise = do
              resp <- LE.catch (if max_redirects==0
                                  then (\(_,a,_) -> a) `fmap` performRequest request'
                                  else runRedirectionChain request' max_redirects [])
                (\ e' -> retryHelper request' (retry_count - 1) max_redirects check_status (Just $ toException (e' :: HttpException)))
              case check_status (HC.responseStatus resp) (responseHeaders resp) of
                Nothing -> return resp
                Just e' -> retryHelper request' (retry_count - 1) max_redirects check_status (Just e')
        performRequest request' = do
              s@(BrowserState { manager = manager'
                              , authorities = auths
                              , cookieJar = cookie_jar
                              , cookieFilter = cookie_filter
                              }) <- get
              now <- liftIO getCurrentTime
              let (request'', cookie_jar') = insertCookiesIntoRequest
                                              (applyAuthorities auths request')
                                              (evictExpiredCookies cookie_jar now) now
              res <- lift $ HC.http request'' manager'
              (cookie_jar'', response) <- liftIO $ updateCookieJar res request'' now cookie_jar' cookie_filter
              put $ s {cookieJar = cookie_jar''}
              return (request'', res, response)
        runRedirectionChain request' redirect_count ress
          | redirect_count == (-1) = case ress of
            [] -> throw $ TooManyRedirects Nothing
            _ -> throw . TooManyRedirects . Just =<< mapM (liftIO . runResourceT . lbsResponse) ress
          | otherwise = do
              (request'', res, response) <- performRequest request'
              let code = HT.statusCode (responseStatus response)
              if code >= 300 && code < 400
                then do request''' <- case HC.getRedirectedRequest request'' (responseHeaders response) code of
                            Just a -> return a
                            Nothing -> throw $ HC.UnparseableRedirect (responseStatus response) (responseHeaders response)
                        runRedirectionChain request''' (redirect_count - 1) (res:ress)
                else return res
        applyAuthorities auths request' = case auths request' of
          Just (user, pass) -> applyBasicAuth user pass request'
          Nothing -> request'

makeRequestLbs :: Request (ResourceT IO) -> BrowserAction (Response L.ByteString)
makeRequestLbs = liftIO . runResourceT . lbsResponse <=< makeRequest

applyOverrideHeaders :: Map.Map HT.HeaderName BS.ByteString -> Request a -> Request a
applyOverrideHeaders ov request' = request' {requestHeaders = x $ requestHeaders request'}
  where x r = Map.toList $ Map.union ov (Map.fromList r)

updateCookieJar :: Response a -> Request (ResourceT IO) -> UTCTime -> CookieJar -> (Request (ResourceT IO) -> Cookie -> IO Bool) -> IO (CookieJar, Response a)
updateCookieJar response request' now cookie_jar cookie_filter = do
  filtered_cookies <- filterM (cookie_filter request') $ catMaybes $ map (\ sc -> generateCookie sc request' now True) set_cookies
  return (cookieJar' filtered_cookies, response {HC.responseHeaders = other_headers})
  where (set_cookie_headers, other_headers) = partition ((== (mk $ fromString "Set-Cookie")) . fst) $ HC.responseHeaders response
        set_cookie_data = map snd set_cookie_headers
        set_cookies = map parseSetCookie set_cookie_data
        cookieJar' = foldl (\ cj c -> insertCheckedCookie c cj True) cookie_jar

-- | You can save and restore the state at will
getBrowserState    :: BrowserAction BrowserState
getBrowserState = get
setBrowserState    :: BrowserState -> BrowserAction ()
setBrowserState = put
withBrowserState   :: BrowserState -> BrowserAction a -> BrowserAction a
withBrowserState s a = do
  current <- get
  put s
  out <- a
  put current
  return out

-- | The number of redirects to allow.
-- if Nothing uses Request's 'redirectCount'
getMaxRedirects    :: BrowserAction (Maybe Int)
getMaxRedirects    = get >>= \ a -> return $ maxRedirects a
setMaxRedirects    :: Maybe Int -> BrowserAction ()
setMaxRedirects  b = get >>= \ a -> put a {maxRedirects = b}
withMaxRedirects   :: Maybe Int -> BrowserAction a -> BrowserAction a
withMaxRedirects a b = do
  current <- getMaxRedirects
  setMaxRedirects a
  out <- b
  setMaxRedirects current
  return out
-- | The number of times to retry a failed connection
getMaxRetryCount   :: BrowserAction Int
getMaxRetryCount   = get >>= \ a -> return $ maxRetryCount a
setMaxRetryCount   :: Int -> BrowserAction ()
setMaxRetryCount b = get >>= \ a -> put a {maxRetryCount = b}
withMaxRetryCount  :: Int -> BrowserAction a -> BrowserAction a
withMaxRetryCount a b = do
  current <- getMaxRetryCount
  setMaxRetryCount a
  out <- b
  setMaxRetryCount current
  return out
-- | Number of microseconds to wait for a response.
-- if Nothing uses Request's 'responseTimeout'
getTimeout         :: BrowserAction (Maybe Int)
getTimeout         = get >>= \ a -> return $ timeout a
setTimeout         :: Maybe Int -> BrowserAction ()
setTimeout       b = get >>= \ a -> put a {timeout = b}
withTimeout        :: Maybe Int -> BrowserAction a -> BrowserAction a
withTimeout    a b = do
  current <- getTimeout
  setTimeout a
  out <- b
  setTimeout current
  return out
-- | A user-provided function that provides optional authorities.
-- This function gets run on all requests before they get sent out.
-- The output of this function is applied to the request.
getAuthorities     :: BrowserAction (Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString))
getAuthorities     = get >>= \ a -> return $ authorities a
setAuthorities     :: (Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)) -> BrowserAction ()
setAuthorities   b = get >>= \ a -> put a {authorities = b}
withAuthorities    :: (Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)) -> BrowserAction a -> BrowserAction a
withAuthorities a b = do
  current <- getAuthorities
  setAuthorities a
  out <- b
  setAuthorities current
  return out
-- | Each new Set-Cookie the browser encounters will pass through this filter.
-- Only cookies that pass the filter (and are already valid) will be allowed into the cookie jar
getCookieFilter    :: BrowserAction (Request (ResourceT IO) -> Cookie -> IO Bool)
getCookieFilter    = get >>= \ a -> return $ cookieFilter a
setCookieFilter    :: (Request (ResourceT IO) -> Cookie -> IO Bool) -> BrowserAction ()
setCookieFilter  b = get >>= \ a -> put a {cookieFilter = b}
withCookieFilter   :: (Request (ResourceT IO) -> Cookie -> IO Bool) -> BrowserAction a -> BrowserAction a
withCookieFilter a b = do
  current <- getCookieFilter
  setCookieFilter a
  out <- b
  setCookieFilter current
  return out
-- | All the cookies!
getCookieJar       :: BrowserAction CookieJar
getCookieJar       = get >>= \ a -> return $ cookieJar a
setCookieJar       :: CookieJar -> BrowserAction ()
setCookieJar     b = get >>= \ a -> put a {cookieJar = b}
withCookieJar      :: CookieJar -> BrowserAction a -> BrowserAction a
withCookieJar a b = do
  current <- getCookieJar
  setCookieJar a
  out <- b
  setCookieJar current
  return out
-- | An optional proxy to send all requests through
-- if Nothing uses Request's 'proxy'
getCurrentProxy    :: BrowserAction (Maybe Proxy)
getCurrentProxy    = get >>= \ a -> return $ currentProxy a
setCurrentProxy    :: Maybe Proxy -> BrowserAction ()
setCurrentProxy  b = get >>= \ a -> put a {currentProxy = b}
withCurrentProxy   :: Maybe Proxy -> BrowserAction a -> BrowserAction a
withCurrentProxy a b = do
  current <- getCurrentProxy
  setCurrentProxy a
  out <- b
  setCurrentProxy current
  return out
-- | An optional SOCKS proxy to send all requests through
-- if Nothing uses Request's 'socksProxy'
getCurrentSocksProxy    :: BrowserAction (Maybe SocksConf)
getCurrentSocksProxy    = get >>= \ a -> return $ currentSocksProxy a
setCurrentSocksProxy    :: Maybe SocksConf -> BrowserAction ()
setCurrentSocksProxy  b = get >>= \ a -> put a {currentSocksProxy = b}
withCurrentSocksProxy   :: Maybe SocksConf -> BrowserAction a -> BrowserAction a
withCurrentSocksProxy a b = do
  current <- getCurrentSocksProxy
  setCurrentSocksProxy a
  out <- b
  setCurrentSocksProxy current
  return out
-- | Specifies Headers that should be added to 'Request',
-- these will override Headers already specified in 'requestHeaders'.
--
-- > do insertOverrideHeader ("User-Agent", "http-conduit")
-- >    insertOverrideHeader ("Connection", "keep-alive")
-- >    makeRequest def{requestHeaders = [("User-Agent", "another agent"), ("Accept", "everything/digestible")]}
-- > > User-Agent: http-conduit
-- > > Accept: everything/digestible
-- > > Connection: keep-alive
getOverrideHeaders :: BrowserAction HT.RequestHeaders
getOverrideHeaders = get >>= \ a -> return $ Map.toList $ overrideHeaders a
setOverrideHeaders :: HT.RequestHeaders -> BrowserAction ()
setOverrideHeaders b = do
    current_user_agent <- getUserAgent
    get >>= \ a -> put a {overrideHeaders = Map.fromList b}
    setUserAgent current_user_agent
withOverrideHeaders:: HT.RequestHeaders -> BrowserAction a -> BrowserAction a
withOverrideHeaders a b = do
  current <- getOverrideHeaders
  setOverrideHeaders a
  out <- b
  setOverrideHeaders current
  return out
insertOverrideHeader :: HT.Header -> BrowserAction ()
insertOverrideHeader (b, c) = get >>= \ a -> put a {overrideHeaders = Map.insert b c (overrideHeaders a)}
deleteOverrideHeader :: HT.HeaderName -> BrowserAction ()
deleteOverrideHeader b = get >>= \ a -> put a {overrideHeaders = Map.delete b (overrideHeaders a)}
withOverrideHeader :: HT.Header -> BrowserAction a -> BrowserAction a
withOverrideHeader a b = do
  current <- getOverrideHeaders
  insertOverrideHeader a
  out <- b
  setOverrideHeaders current
  return out
-- | What string to report our user-agent as.
-- if Nothing will not send user-agent unless one is specified in 'Request'
--
-- > getUserAgent = lookup hUserAgent overrideHeaders
-- > setUserAgent a = insertOverrideHeader (hUserAgent, a)
getUserAgent       :: BrowserAction (Maybe BS.ByteString)
getUserAgent       = get >>= \ a -> return $ Map.lookup HT.hUserAgent (overrideHeaders a)
setUserAgent       :: Maybe BS.ByteString -> BrowserAction ()
setUserAgent Nothing = deleteOverrideHeader HT.hUserAgent
setUserAgent (Just b) = insertOverrideHeader (HT.hUserAgent, b)
withUserAgent      :: Maybe BS.ByteString -> BrowserAction () -> BrowserAction ()
withUserAgent a b = do
  current <- getOverrideHeaders
  setUserAgent a
  out <- b
  setOverrideHeaders current
  return out
-- | Function to check the status code. Note that this will run after all redirects are performed.
-- if Nothing uses Request's 'checkStatus'
getCheckStatus    :: BrowserAction (Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException))
getCheckStatus    = get >>= \ a -> return $ browserCheckStatus a
setCheckStatus    :: Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException) -> BrowserAction ()
setCheckStatus  b = get >>= \ a -> put a {browserCheckStatus = b}
withCheckStatus   :: Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException) -> BrowserAction a -> BrowserAction a
withCheckStatus a b = do
  current <- getCheckStatus
  setCheckStatus a
  out <- b
  setCheckStatus current
  return out
-- | Whether to include response bodies in 'HttpException'
-- if Nothing uses Request's 'debug'
getDebug    :: BrowserAction (Maybe Bool)
getDebug    = get >>= \ a -> return $ browserDebug a
setDebug    :: Maybe Bool -> BrowserAction ()
setDebug  b = get >>= \ a -> put a {browserDebug = b}
withDebug   :: Maybe Bool -> BrowserAction a -> BrowserAction a
withDebug a b = do
  current <- getDebug
  setDebug a
  out <- b
  setDebug current
  return out

-- | The active manager, managing the connection pool
getManager         :: BrowserAction Manager
getManager         = get >>= \ a -> return $ manager a
setManager         :: Manager -> BrowserAction ()
setManager       b = get >>= \ a -> put a {manager = b}
