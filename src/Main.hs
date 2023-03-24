{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Amazonka hiding (Header)
import Amazonka.S3
import Amazonka.S3.Lens
import Amazonka.Polly
import Amazonka.Polly.SynthesizeSpeech
import Configuration.Dotenv
import Control.Lens
import Control.Monad (void)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Conduit
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Combinators
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Conduit ()
import qualified Servant.Types.SourceT as S

main :: IO ()
main = run 3030 app

app :: Application
app = serve (Proxy @API) server

type API =
  "one-step" :> StreamGet NoFraming OctetStream (ConduitT () ByteString (ResourceT IO) ())

server :: Server API
server = oneStepHandler

getObjectRequest :: GetObject
getObjectRequest = newGetObject (BucketName "amazonka-servant-streaming") (ObjectKey "haskell.png")

-- forwarding the stream
oneStepHandler :: Handler (ConduitT () ByteString (ResourceT IO) ())
oneStepHandler = do
  awsEnv <- newEnv discover
  resourceState <- createInternalState
  res <- flip runInternalState resourceState $ do
    awsRes <- send awsEnv (newSynthesizeSpeech OutputFormat_Mp3 "hello" VoiceId_Joanna)
    -- awsRes <- send awsEnv getObjectRequest
    pure $ awsRes ^. synthesizeSpeechResponse_audioStream ^. _ResponseBody

  -- It took me a long time to understand what this was doing.
  -- Because ConduitT is a monad transformer and we have
  -- closeInternalState :: MonadIO m => InternalState -> m ()
  -- this attaches the `closeInternalState resourceState` action to the
  -- end of the conduit, just before it returns its final value
  -- (which was () anyway).
  pure $ res *> closeInternalState resourceState
