module Main where

import qualified Data.ByteString as B
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import System.Posix.User (UserEntry(userID, userGroupID), getUserEntryForName, setUserID, setGroupID)

import Gopher

main :: IO ()
main = gopher "barrucadu.co.uk" 70 serve $ do
  -- Because we need to be root to listen on port 70, switch the uid/gid to nobody.
  nobody <- getUserEntryForName "nobody"
  setGroupID (userGroupID nobody)
  setUserID  (userID      nobody)

serve :: String -> IO (Maybe Response)
serve ('/':rest) = serve rest
serve ('.':_) = pure Nothing
serve ""  = Just . DirListing <$> dirFile "root.gopher"
serve selector = do
  let fname = map (\c -> if c == '/' then '-' else c) selector
  fileExists   <- doesFileExist fname
  gopherExists <- doesFileExist (fname ++ ".gopher")
  if gopherExists
  then Just . DirListing <$> dirFile (fname ++ ".gopher")
  else if fileExists
       then if takeExtension fname `elem` textFileExts
            then Just . Text   <$> readFile   fname
            else Just . Binary <$> B.readFile fname
       else pure Nothing

textFileExts :: [String]
textFileExts = [".txt", ".md", ".markdown"]
