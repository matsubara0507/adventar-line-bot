{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude            hiding (unlines)

import           Control.Exception  (IOException, catch)
import           Control.Monad      (unless)
import           Data.Either        (isLeft)
import           Data.Text          (Text, pack, unlines, unpack)
import           Entry
import           Html
import           Json
import           Line
import           Scraper
import           System.Environment (getArgs)

main :: IO ()
main = do
  [htmlUrl, jsonPath, token, mid] <- fmap pack <$> getArgs
  catch (runBot htmlUrl jsonPath token mid) $
    \e -> putStrLn ("Error: " `mappend` show (e :: IOException))

runBot :: Url -> Text -> Text -> Text -> IO ()
runBot htmlUrl jsonPath token mid = do
  oldCal <- readEntryJson jsonPath
  newCal <- adventarScraper <$> fetchHtml htmlUrl

  let
    messages = mkMessages oldCal newCal

  unless (null messages) $ do
    result <- pushMessage token mid . unlines $
      "更新がありました！" : htmlUrl : messages
    unless (isLeft result) $ updateEntryJson jsonPath newCal

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
