{-# LANGUAGE LambdaCase #-}
module Gopher where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString, hPut)
import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Network
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, isSymbolicLink)
import System.FilePath ((</>), joinPath, takeExtension)
import System.IO (hClose, hGetChar, hPutStr)
import System.Posix.Files (readSymbolicLink)
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

-- | A sensible default request handler.
--
-- Leading slashes are stripped, and the path is interpreted as a
-- filesystem path relative to the current working directory (with
-- accessing hidden files/directories forbidden, and symlinks
-- followed). If it's a directory and a \".gopher\" file exists in
-- that directory, it is served; otherwise a directory listing is
-- generated (which excludes hidden files).
defaultServe :: String -> IO (Maybe Response)
defaultServe selector =
  let segments = splitBy '/' selector
      path = joinPath segments
  in if any ("." `isPrefixOf`) segments
     then pure Nothing
          else servePath path path
  where
    servePath orig "" = serveDir orig ""
    servePath orig path = getPathType path >>= \case
      Just (Symbolic path') -> servePath orig path'
      Just File      -> serveFile path
      Just Directory -> serveDir  orig path
      Nothing -> pure Nothing

    serveFile path
      | getTypeByExtension (map toLower $ takeExtension path) == '0' = Just . Text <$> readFile path
      | otherwise = Just . Binary <$> B.readFile path

    serveDir orig path = do
      let dirPath = "." </> path
      let gopherPath = dirPath </> ".gopher"
      gopherExists <- doesFileExist gopherPath
      if gopherExists
      then Just . DirListing <$> dirFile gopherPath
      else Just . DirListing . mapMaybe (dirEntry orig) <$> listDirectory dirPath

    dirEntry _ ('.':_) = Nothing
    dirEntry path fname =
      let ext = map toLower (takeExtension fname)
          sel = path </> fname
          ty = getTypeByExtension ext
      in Just (DirEntry ty fname sel Nothing Nothing)

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

-- | Get the type of a file by its extension.
getTypeByExtension :: String -> Char
getTypeByExtension ext
  | ext `elem` textFileExts = '0'
  | ext == ".gif" = 'g'
  | ext == ".html" = 'h'
  | ext `elem` imageFileExts = 'I'
  | ext `elem` audioFileExts = 's'
  | otherwise = '9'

  where
    textFileExts  = [".css", ".md", ".markdown", ".rst", ".txt"]
    imageFileExts = [".bmp", ".jpg", ".jpeg", ".png", ".svg", ".tiff", ".webp"]
    audioFileExts = [".flac", ".m4a", ".mp3", ".ogg", ".wav"]

data PathType = Symbolic FilePath | Directory | File
  deriving Show

-- | Get the type of a file.
getPathType :: FilePath -> IO (Maybe PathType)
getPathType path = do
  isSymLink <- isSymbolicLink     path
  isFile    <- doesFileExist      path
  isDir     <- doesDirectoryExist path

  case (isSymLink, isFile, isDir) of
    (True, _, _) -> Just . Symbolic <$> readSymbolicLink path
    (_, True, _) -> pure (Just File)
    (_, _, True) -> pure (Just Directory)
    _ -> pure Nothing
