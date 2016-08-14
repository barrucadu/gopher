module Gopher where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString, hPut)
import Data.Maybe (fromMaybe, mapMaybe)
import Network
import System.IO (hClose, hGetChar, hPutStr)
import Text.Read (readMaybe)

data Response
  = DirListing [DirEntry]
  | Text String
  | Binary ByteString
  deriving Show

data DirEntry = DirEntry
  { eType     :: Char
  , eName     :: String
  , eSelector :: String
  , eHost     :: Maybe HostName
  , ePort     :: Maybe PortNumber
  }
  deriving Show

-- | Run a Gopher server with the given host and port.
gopher :: HostName -> PortNumber -> (String -> IO (Maybe Response)) -> IO () -> IO ()
gopher host port handle afterConnect = do
  sock <- listenOn (PortNumber port)
  afterConnect
  forever (forkIO . serve =<< accept sock)

  where
    serve (h, remHost, _) = do
      query <- hGetSelector h
      putStrLn $ remHost ++ ": " ++ show query
      response <- handle query
      case response of
        Just (DirListing entries) -> do
          mapM_ (hPutLn h . dirEntry) entries
          hPutLn h "."
        Just (Text str) -> do
          mapM_ (hPutLn h . textEntry) (lines str)
          hPutLn h "."
        Just (Binary bs) -> hPut h bs
        Nothing -> pure ()
      hClose h

    dirEntry entry = concat [ eType entry : eName entry, "\t"
                            , eSelector entry, "\t"
                            , fromMaybe host (eHost entry), "\t"
                            , show $ fromMaybe port (ePort entry)
                            ]

    -- A line starting with a '.' gets prefixed with another '.', to avoid early connection termination by the client.
    textEntry l@('.':_) = '.':l
    textEntry l = l

    -- A selector runs up to a \r, \n, or a \t, whichever comes first.
    hGetSelector h = go (1024::Int) where
      -- Character limit.
      go 0 = pure ""
      go n = do
        c <- hGetChar h
        if c `elem` "\t\r\n"
          then pure ""
          else (c:) <$> go (n-1)

    hPutLn h s = hPutStr h s >> hPutStr h "\r\n"

-- | Read a directory listing file.
dirFile :: FilePath -> IO [DirEntry]
dirFile fp = mapMaybe dirEntry . lines <$> readFile fp where
  dirEntry [] = Nothing
  dirEntry (ty:s) = case splitBy '\t' s of
    (name:sel:host:port:_) -> DirEntry ty name sel (Just host) <$> readMaybe port
    [name, sel, host] -> Just (DirEntry ty name sel (Just host) Nothing)
    [name, sel] -> Just (DirEntry ty name sel Nothing Nothing)
    [name]
      | ty == 'i' -> Just (DirEntry ty name ""   Nothing Nothing)
      | otherwise -> Just (DirEntry ty name name Nothing Nothing)
    []
      | ty == 'i' -> Just (DirEntry ty s "" Nothing Nothing)
      | otherwise -> Nothing

-- | Split a string by a delimiter. It never ceases to amaze me that
-- this isn't standard.
splitBy :: Char -> String -> [String]
splitBy delim s = case dropWhile (==delim) s of
  "" -> []
  s' -> let (w, s'') = break (==delim) s'
        in w : splitBy delim s''
