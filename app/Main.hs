{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Debug.Trace
import Prelude hiding (unlines)
import Unsafe.Coerce
import Control.Concurrent
import Data.Text hiding (pack, map)
import Data.Function
import Hledger.Read (readJournal)
import Hledger.Data.Account
import Hledger.Data.Journal
import Hledger.Data.Transaction
import Hledger.Data.Types
import GHCJS.Types
import GHCJS.Foreign.Callback
import Miso
import Miso.String (pack, toMisoString, fromMisoString, MisoString)

import qualified Data.JSString
import qualified Data.Text

data Model
  = Model
    { journalSource :: MisoString
    , currentlySaved :: MisoString
    , journal :: Maybe Journal
    , status :: Maybe MisoString
    , err :: Maybe MisoString
    , waitingDebounced :: Int
    } deriving (Eq, Show)

main :: IO ()
main = do
  startApp App {..}
  where
    initialAction = FetchInitial
    model         = Model
      { journalSource    = initialJournal
      , currentlySaved   = initialJournal
      , journal          = Nothing
      , status           = Nothing
      , err              = Nothing
      , waitingDebounced = 0
      }
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    mountPoint    = Nothing
    subs          = [ getSub ]

data Action
  = NoOp
  | FetchInitial
  | GotValue (Text, Text)
  | SetUnparsedJournal MisoString
  | MaybeParseJournal
  | ImmediatelyParseJournal
  | ParsedJournal (Either String Journal)
  | SaveCurrent
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m

updateModel FetchInitial m = m <# do
  rsGet "main.journal"
  pure ImmediatelyParseJournal

updateModel (GotValue (path, contents)) m =
  let jnrl = toMisoString contents
  in m
    { journalSource = jnrl
    , currentlySaved = jnrl
    } <# do
      pure ImmediatelyParseJournal

updateModel (SetUnparsedJournal uj) m@Model{..} =
  m
    { journalSource = uj
    , waitingDebounced = waitingDebounced + 1
    , status = Just $ toMisoString $ Data.Text.concat
      [ "waiting to parse..."
      , Data.Text.pack $ show $ waitingDebounced + 1
      ]
    } <# do
      threadDelay 800000 -- 0.8s
      pure MaybeParseJournal

updateModel MaybeParseJournal (Model {..}) =
  Model { waitingDebounced = waitingDebounced - 1, .. }
    <# do
      if waitingDebounced == 1
        then pure ImmediatelyParseJournal
        else pure NoOp

updateModel ImmediatelyParseJournal (Model {..}) =
  Model { status = Just $ pack "parsing journal...", .. }
    <# do
      ParsedJournal <$> parseJournal journalSource

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
        , status = Just $ pack "finished parsing!"
        }

updateModel SaveCurrent (Model {..}) = Model {..} <# do
  rsPut "main.journal" (fromMisoString journalSource)
  pure NoOp

viewModel :: Model -> View Action
viewModel Model {..} = div_ []
  [ nav_ [ class_ "navbar" ]
    [ div_ [ class_ "navbar-brand" ]
      [ a_ [ class_ "navbar-item" ] [ text $ pack "d" ]
      ]
    , div_ [ class_ "navbar-menu" ]
      [ div_ [ class_ "navbar-start" ] []
      , div_ [ class_ "navbar-end" ]
        [ a_ [ id_ "rs-widget", class_ "navbar-item" ] [ text $ pack "" ]
        ]
      ]
    ]
  , div_ [ class_ "container" ]
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
          ] [ text journalSource ]
        , button_
          [ class_ "button is-primary"
          , disabled_ $ currentlySaved == journalSource
          , onClick SaveCurrent
          ] [ text $ pack "Save" ]
        ]
      , div_ [ class_ "column" ]
        $ case journal of
          Nothing -> []
          Just jrnl ->
            [ div_ []
              [ h4_ [ class_ "title is-4" ] [ text $ pack "Account Balances" ]
              , viewAccountTree $ Prelude.tail $ accountTree jrnl
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
  ]

viewAccountTree :: [Account] -> View Action
viewAccountTree accounts =
  div_ [ class_ "columns is-multiline account-tree" ]
    $ map viewAccount accounts

viewAccount :: Account -> View Action
viewAccount acc@Account{..} =
  let
    offset = (parentAccounts acc & Prelude.length) - 1
    size = 12 - offset
    cls = "column is-" ++ show size ++ " is-offset-" ++ show offset
  in div_ [ class_ $ toMisoString cls ]
    [ div_ [ class_ "columns" ]
      [ div_ [ class_ "column" ] [ text $ toMisoString aname ]
      , viewAmounts aebalance
      ]
    ]

viewAmounts :: MixedAmount -> View Action
viewAmounts (Mixed amts) =
  div_ [ class_ "column is-narrow" ] $ map viewAmount amts

viewAmount :: Amount -> View Action
viewAmount (Amount {..}) =
  div_ []
    [ text $ toMisoString acommodity
    , text $ pack " "
    , text $ pack $ show aquantity
    ]


foreign import javascript unsafe
  "setGetHandler($1)"
  onGet :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "client.getFile($1).then(function (res) { console.log(res.data); getHandler($1, res.data) })"
  rsGet :: JSString -> IO ()

foreign import javascript unsafe
  "client.storeFile('text/plain', $1, $2)"
  rsPut :: JSString -> JSString -> IO ()

foreign import javascript unsafe
  "client.remove($1)"
  rsDel :: JSString -> IO ()

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

accountTree :: Journal -> [Account]
accountTree = accountsFromPostings . journalPostings

getSub :: Sub Action Model
getSub _ = \sink -> do
  onGet =<< do
    asyncCallback2 $ \p v -> do
      let path = Data.Text.pack $ Data.JSString.unpack ((unsafeCoerce p)::JSString)
      let contents = Data.Text.pack $ Data.JSString.unpack ((unsafeCoerce v)::JSString)
      sink $ GotValue (path, contents)
