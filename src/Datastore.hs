{-# LANGUAGE OverloadedStrings #-}

module Datastore where

import           Control.Lens             ((&), (.~), (<&>), (?~), (^.))
import qualified Data.HashMap.Lazy        as HM
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.Text                (Text, append)
import           Network.Google
import           Network.Google.Datastore

getMids :: Text -> Text -> IO [Text]
getMids projectId kind = do
  env <- newEnv <&> envScopes .~ datastoreScope
  let
    request = runQueryRequest &
     (rqrPartitionId ?~ (partitionId & piProjectId ?~ projectId)) . (rqrGqlQuery ?~ (gqlQuery & gqQueryString ?~ append "SELECT * FROM " kind))
  response <- runResourceT . runGoogle env $ send (projectsRunQuery request projectId)
  return . catMaybes . fromMaybe [] $ do
    batch <- response ^. rBatch
    return $ getMid <$> batch ^. qrbEntityResults

getMid :: EntityResult -> Maybe Text
getMid result = do
  entity' <- result ^. erEntity
  properties <- entity' ^. eProperties
  property <- HM.lookup "mid" $ properties ^. epAddtional
  property ^. vStringValue
