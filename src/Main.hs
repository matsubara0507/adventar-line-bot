{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception  (IOException, catch)
import           Control.Monad      (when)
import           Data.Either        (isRight)
import           Data.List          (intersperse)
import           Data.Text          (Text, pack, unpack)
import           Entry
import           Html
import           Json
import           Scraper
import           System.Environment (getArgs)

main :: IO ()
main = do
  [htmlUrl, jsonPath] <- fmap pack <$> getArgs
  catch (runBot htmlUrl jsonPath) $
    \e -> putStrLn ("Error: " `mappend` show (e :: IOException))

runBot :: Url -> Text -> IO ()
runBot htmlUrl jsonPath = do
  oldCal <- readEntryJson jsonPath
  newCal <- adventarScraper <$> fetchHtml htmlUrl

  let
    message = mkMessage oldCal newCal

  putStrLn $ either id id message

  when (isRight message) $ updateEntryJson jsonPath newCal


mkMessage :: Calendar -> Calendar -> Either String String
mkMessage oldCal newCal =
  if null message then Left "No update..." else Right message
  where
    message = unlines . filter ("" /=) $ mkMessage' <$> dates
    mkMessage' date = diffShow date oldCal newCal

diffShow :: Date -> Calendar -> Calendar -> String
diffShow date = (.) (diffShow' date) . diff date

diffShow' :: Date -> DiffEntry -> String
diffShow' date diffEntry =
  case diffEntry of
    NewEntry newEntry ->
      mconcat [unpack date, " [New] ", ppEntry newEntry]
    UpdateBody newEntry ->
      mconcat [unpack date, " [Update] ", ppEntry newEntry]
    RemoveEntry oldEntry ->
      mconcat [unpack date, " [Remove] ", ppEntry oldEntry]
    ChangeUser oldEntry newEntry ->
      mconcat . intersperse "\n" $
        diffShow' date <$> [RemoveEntry oldEntry, NewEntry newEntry]
    NoChanged -> ""

ppEntry :: Entry -> String
ppEntry (Entry user' comment' title' url') =
  mconcat [mkBody comment' title' url', " by ", unpack user']
  where
    mkBody "" _ ""          = unpack "No Comment..."
    mkBody comment_ _ ""    = unpack comment_
    mkBody comment_ "" url_ = unpack $ mconcat ["<", url_, "|", comment_, ">"]
    mkBody _ title_ url_    = unpack $ mconcat ["<", url_, "|", title_, ">"]
