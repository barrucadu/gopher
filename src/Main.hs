module Main where

import qualified Data.ByteString as B
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

import Gopher

main :: IO ()
main = gopher "barrucadu.co.uk" 70 serve

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
