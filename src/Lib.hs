module Lib
    ( runTCPServer
    , echo
    , Response(..)
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Parser (runParser, string)



runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) mhost (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \soc -> do
      setSocketOption soc ReuseAddr 1
      withFdSocket soc setCloseOnExecIfNeeded
      bind soc $ addrAddress addr
      listen soc 1024
      return soc

    loop soc = forever $ E.bracketOnError (accept soc) (close . fst)
      $ \(conn, _peer) -> void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
      
echo :: Socket -> IO ()
echo s = do
  msg <- recv s 1024
  unless (S.null msg) $ do
    sendAll s msg
    echo s



data Request
data Response = SimpleS String
              | SimpleE String
              | Integer Int


instance Show Response where
  show (SimpleS s) = "+" ++ s ++ "\r\n"
  show (SimpleE s) = "-" ++ s ++ "\r\n"
  show (Integer i) = ":" ++ show i ++ "\r\n"

simpleS :: String -> Response
simpleS = SimpleS

simpleE :: String -> Response
simpleE = SimpleE

integer :: Int -> Response
integer = Integer


test = runParser (string "Haskell") "Haskell"
