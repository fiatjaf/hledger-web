{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Debug.Trace
import Prelude hiding (unlines)
import Data.Text hiding (pack, map)
import Hledger.Read (readJournal)
import Hledger.Data.Journal
import Hledger.Data.Transaction
import Hledger.Data.Types (Journal)
import GHCJS.Types
import Miso
import Miso.String (pack, toMisoString, fromMisoString, MisoString)

data Model
  = Model
    { unparsed :: MisoString
    , journal :: Maybe Journal
    , err :: Maybe MisoString
    } deriving (Eq, Show)

main :: IO ()
main = startApp App {..}
  where
    initialAction = ParseJournal
    model  = Model initialJournal Nothing Nothing
    update = updateModel
    view   = viewModel
    events = defaultEvents
    mountPoint = Nothing
    subs   = []

data Action
  = NoOp
  | SetUnparsedJournal MisoString
  | ParseJournal
  | ParsedJournal (Either String Journal)
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (SetUnparsedJournal uj) m =
  updateModel ParseJournal m { unparsed = uj }
updateModel ParseJournal (Model {..}) = Model {..} <# do
  putStr (fromMisoString unparsed)
  ParsedJournal <$> parseJournal unparsed
updateModel (ParsedJournal res) m = noEff updatedModel
  where
    updatedModel = case res of
      Left err -> m { err = Just $ pack err }
      Right jrnl -> m { journal = Just jrnl }

viewModel :: Model -> View Action
viewModel Model {..} = div_ []
  [ div_ []
    [ case err of
      Nothing -> text $ pack "no errors" 
      Just err -> text $ toMisoString err
    ]
  , div_ []
    [ div_ []
      [ textarea_ [ onInput SetUnparsedJournal ] [ text unparsed ]
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
  ]

-- foreign import javascript unsafe "$r = new RemoteStorage({logging: true})"
--   newRS :: IO JSVal
-- 
-- foreign import javascript unsafe "$1.access.claim($2, 'rw')"
--   rsClaim :: JSVal -> Text -> IO ()
-- 
-- foreign import javascript unsafe "$1.caching.enable($2)"
--   rsCache :: JSVal -> Text -> IO ()

parseJournal :: MisoString -> IO (Either String Journal)
parseJournal jrnl =
  readJournal (Just "journal") Nothing True Nothing (fromMisoString jrnl)

initialJournal :: MisoString
initialJournal = toMisoString $ unlines
  [ "2018/2/7 got paid"
  , "  assets:money  1200"
  , "  income"
  , ""
  , "2018/1/18 coffee"
  , "  assets:money  -12"
  , "  expenses:food"
  ]
