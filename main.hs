import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Monad
import Control.Concurrent

server = "irc.freenode.org"
port = 6667
chan = "#haskell"
nick = "namosca_test"

main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" (nick ++ " 0 * :tutorial bot")
  write h "JOIN" chan
  mvar <- myForkIO $ listen h --https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent.html#v:forkIO
  mvar2 <- myForkIO $ speak h
  mapM_ takeMVar [mvar,mvar2]
  
    

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

speak:: Handle -> IO()
speak h = forever $ do
  putStrLn "Message: "
  textOut <- getLine
  privmsg h textOut

listen :: Handle -> IO()
listen h = forever $ do
  t <- hGetLine h
  let s = init t
  if ping s then pong s else eval h (clean s)
  putStrLn.show.clean $ s --putStrLn s
  where
    clean = id--drop 1.dropWhile(/= ':').dropWhile(/=' ').dropWhile (/= '!').id -- dropWhile (/= ' ')
    ping x = "PING :" `isPrefixOf` x
    pong x = write h "PONG" (':' : drop 6 x)

eval:: Handle -> String -> IO ()
eval h x | "!quit" `isInfixOf` x= write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x | "!id " `isInfixOf` x = privmsg h (drop 4 x)
eval _ _ = return ()

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

myForkIO :: IO() -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  forkFinally io (\_ -> putMVar mvar ())
  return mvar
