{-# LANGUAGE OverloadedStrings #-}

module Datastore where

import           Control.Lens             (view, (&), (.~), (<&>), (?~), (^.))
import qualified Data.HashMap.Lazy        as HM
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.Text                (Text, append)
import           Network.Google
import           Network.Google.Datastore

getMids :: Text -> Text -> IO [Text]
getMids projectId kind = do
  env <- newEnv <&> envScopes .~ datastoreScope
  let
    request = runQueryRequest
      & rqrPartitionId ?~ (partitionId & piProjectId ?~ projectId)
      & rqrGqlQuery ?~ (gqlQuery & gqQueryString ?~ append "SELECT * FROM " kind)
  response <-
    runResourceT . runGoogle env $ send (projectsRunQuery request projectId)
  return . catMaybes . fromMaybe [] $
    fmap getMid . view qrbEntityResults <$> response ^. rBatch

getMid :: EntityResult -> Maybe Text
getMid result =
  result ^. erEntity >>= view eProperties
    <&> view epAddtional >>= HM.lookup "mid" >>= view vStringValue
