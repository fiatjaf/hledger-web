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
    , status :: Maybe MisoString
    , err :: Maybe MisoString
    } deriving (Eq, Show)

main :: IO ()
main = startApp App {..}
  where
    initialAction = ParseJournal
    model  = Model initialJournal Nothing Nothing Nothing
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
updateModel (SetUnparsedJournal uj) m = updateModel ParseJournal m { unparsed = uj }
updateModel ParseJournal (Model {..}) =
  Model
    { status = Just $ pack "parsing journal..."
    , ..
    } <# do
  ParsedJournal <$> parseJournal unparsed
updateModel (ParsedJournal res) m = noEff updated
  where
    updated = case res of
      Left err -> m
        { err = Just $ pack err
        , journal = Nothing
        , status = Just $ pack "encountered error while parsing."
        }
      Right jrnl -> m
        { journal = Just jrnl
        , err = Nothing
        , status = Just $ pack "finished parsing."
        }

viewModel :: Model -> View Action
viewModel Model {..} = div_ []
  [ div_ [ class_ "columns" ]
    [ div_ [ class_ "column" ]
      [ case err of
        Nothing -> text $ pack "" 
        Just err -> text err
      ]
    , div_ [ class_ "column" ]
      [ case status of
        Nothing -> text $ pack ""
        Just st -> text st
      ]
    ]
  , div_ [ class_ "columns" ]
    [ div_ [ class_ "column" ]
      [ textarea_
        [ onInput SetUnparsedJournal
        , class_ "textarea"
        ] [ text unparsed ]
      ]
    , div_ [ class_ "column" ]
      $ case journal of
        Nothing -> []
        Just jrnl ->
          [ div_ []
            [ h4_ [ class_ "title is-4" ] [ text $ pack "account names" ]
            , ul_ []
              $ map (li_ [] . (:[]) . text . toMisoString) (journalAccountNames jrnl)
            ]
          , div_ []
            [ h4_ [ class_ "title is-4" ] [ text $ pack "first transaction" ]
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
  readJournal (Just "journal") Nothing False Nothing (fromMisoString jrnl)

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
