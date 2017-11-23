module Line where

import           Data.Text                (Text)
import           Line.Messaging.API       hiding (Text (..))
import qualified Line.Messaging.API.Types as LINE

pushMessage :: ChannelAccessToken -> ID -> Text -> IO (Either APIError ())
pushMessage token mid message =
  runAPI (pure token) $ push mid [Message $ LINE.Text message]
