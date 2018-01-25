{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Debug.Trace
import Data.Text hiding (pack, map)
import Hledger.Read (readJournal)
import Hledger.Data.Journal
import Hledger.Data.Transaction
import Hledger.Data.Types (Journal)
import GHCJS.Types
import Miso
import Miso.String (pack, toMisoString, MisoString)

data Model
  = Model
    { journal :: Maybe Journal
    , err :: Maybe MisoString
    } deriving (Eq, Show)

main :: IO ()
main = startApp App {..}
  where
    initialAction = ParseJournal
    model  = Model Nothing Nothing
    update = updateModel
    view   = viewModel
    events = defaultEvents
    mountPoint = Nothing
    subs   = []

data Action
  = NoOp
  | SayHelloWorld
  | ParseJournal
  | ParsedJournal (Either String Journal)
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  putStrLn "Hello World!" >> pure NoOp
updateModel ParseJournal m = m <# do
  ParsedJournal <$> parseJournal "\
\2018/1/1 nada \n\
\  jambo  12 \n\
\  caca \n"
updateModel (ParsedJournal res) m = noEff updatedModel
  where
    updatedModel = case res of
      Left err -> m { err = Just $ pack err }
      Right jrnl -> m { journal = Just jrnl }

viewModel :: Model -> View Action
viewModel Model {..} = div_ []
  [ button_ [ onClick SayHelloWorld ] [ text $ pack "hello" ]
  , div_ []
    [ case err of
      Nothing -> text $ pack "no errors" 
      Just err -> text $ toMisoString err
    ]
  , div_ []
    $ case journal of
      Nothing -> []
      Just jrnl ->
        [ div_ []
          [ h1_ [] [ text $ pack "account names" ]
          , ul_ []
            $ map (li_ [] . (:[]) . text . toMisoString) (journalAccountNames jrnl)
          ]
        , div_ []
          [ h1_ [] [ text $ pack "first transaction" ]
          , ul_ []
            [ case journalTransactionAt jrnl 1 of
              Nothing -> text $ pack ""
              Just txn -> pre_ [] [ text $ toMisoString (showTransaction txn) ]
            ]
          ]
        ]
  ]

-- foreign import javascript unsafe "$r = new RemoteStorage({logging: true})"
--   newRS :: IO JSVal
-- 
-- foreign import javascript unsafe "$1.access.claim($2, 'rw')"
--   rsClaim :: JSVal -> Text -> IO ()
-- 
-- foreign import javascript unsafe "$1.caching.enable($2)"
--   rsCache :: JSVal -> Text -> IO ()

parseJournal :: Text -> IO (Either String Journal)
parseJournal jrnl = readJournal (Just "journal") Nothing True Nothing jrnl
