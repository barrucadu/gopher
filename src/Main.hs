module Main where

import System.Posix.User (UserEntry(userID, userGroupID), getUserEntryForName, setUserID, setGroupID)

import Gopher

main :: IO ()
main = gopher "barrucadu.co.uk" 70 defaultServe $ do
  -- Because we need to be root to listen on port 70, switch the uid/gid to nobody.
  nobody <- getUserEntryForName "nobody"
  setGroupID (userGroupID nobody)
  setUserID  (userID      nobody)
