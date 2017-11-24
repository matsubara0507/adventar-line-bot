{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude            hiding (unlines)

import           Control.Monad      (unless)
import           Data.Text          (Text, pack, unlines)
import           Datastore
import           Entry
import           Html
import           Json
import           Line
import           Scraper
import           System.Environment (getArgs, getEnv)

import           GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8
  [htmlUrl, jsonPath, dsKind] <- fmap pack <$> getArgs
  [token, projectId, wdHost, wdPort] <- 
    fmap pack <$> mapM getEnv ["LINE_TOKEN", "PROJECT_ID", "WD_HOST", "WD_PORT"]
  runBot jsonPath htmlUrl (wdHost, wdPort) token (projectId, dsKind)

runBot :: Text -> Url -> (Text, Text) -> Text -> (Text, Text) -> IO ()
runBot jsonPath htmlUrl (wdHost, wdPort) token (projectId, dsKind) = do
  oldCal <- readEntryJson jsonPath
  newCal <- adventarScraper <$> fetchHtml wdHost wdPort htmlUrl

  let
    messages = mkMessages oldCal newCal

  unless (null messages) $ do
    let
      message = unlines $ "更新がありました！" : htmlUrl : messages
    mids <- getMids projectId dsKind
    mapM_ (\mid -> pushMessage token mid message) mids
    updateEntryJson jsonPath newCal

mkMessages :: Calendar -> Calendar -> [Text]
mkMessages oldCal newCal = mconcat $ mkMessage' <$> dates
  where
    mkMessage' date = diffShow date oldCal newCal

diffShow :: Date -> Calendar -> Calendar -> [Text]
diffShow date = (.) (diffShow' date) . diff date

diffShow' :: Date -> DiffEntry -> [Text]
diffShow' date diffEntry =
  case diffEntry of
    NewEntry newEntry ->
      [mconcat [date, " [New] ", ppEntry newEntry]]
    UpdateBody newEntry ->
      [mconcat [date, " [Update] ", ppEntry newEntry]]
    RemoveEntry oldEntry ->
      [mconcat [date, " [Remove] ", ppEntry oldEntry]]
    ChangeUser oldEntry newEntry ->
      mconcat . diffShow' date <$> [RemoveEntry oldEntry, NewEntry newEntry]
    NoChanged -> []

ppEntry :: Entry -> Text
ppEntry (Entry user' comment' title' _url) =
  mconcat [mkBody comment' title', " by ", user']
  where
    mkBody "" _        = "No Comment..."
    mkBody comment_ "" = comment_
    mkBody _ title_    = title_
