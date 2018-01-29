{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Prelude hiding (unlines, replicate)
import Control.Concurrent
import Data.Text hiding (pack, map)
import Data.Function
import Data.JSString.Text (textFromJSVal)
import JavaScript.Array.Internal
import Hledger.Read (readJournal)
import Hledger.Data.Account
import Hledger.Data.AccountName
import Hledger.Data.Journal
import Hledger.Data.Transaction
import Hledger.Data.Types
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import Miso
import Miso.String (pack, toMisoString, fromMisoString, MisoString)

import qualified Data.Text

data Model
  = Model
    { currentJournal :: (MisoString, MisoString)
    , journalsAvailable :: [MisoString]
    , currentlySaved :: (MisoString, MisoString)
    , parsedJournal :: Maybe Journal
    , logged :: Bool
    , showingFileList :: Bool
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
      { currentJournal    = ("temp.journal", initialJournal)
      , journalsAvailable = []
      , currentlySaved    = ("temp.journal", initialJournal)
      , parsedJournal     = Nothing
      , logged            = False
      , showingFileList   = False
      , status            = Nothing
      , err               = Nothing
      , waitingDebounced  = 0
      }
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    mountPoint    = Nothing
    subs          = [ getSub, listSub, loggedSub ]

data Action
  = NoOp
  | FetchInitial
  | GotLogged Bool
  | GotValue (Text, Text)
  | GotListing [Text]
  | SetFileName MisoString
  | OpenFile MisoString
  | ShowFileList Bool
  | SetUnparsedJournal MisoString
  | MaybeParseJournal
  | ImmediatelyParseJournal
  | ParsedJournal (Either String Journal)
  | SaveCurrent
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m

updateModel FetchInitial m@Model{..} = m <# do
  rsList
  rsRetrieve $ fst currentJournal
  pure ImmediatelyParseJournal

updateModel (GotValue (p, contents)) m =
  let
    jnrl = toMisoString contents
    path = toMisoString p
  in m
    { currentJournal = (path, jnrl)
    , currentlySaved = (path, jnrl)
    , showingFileList = False
    } <# do
      pure ImmediatelyParseJournal

updateModel (GotLogged b) m = noEff m { logged = b }

updateModel (GotListing jnrls) m = noEff m
  { journalsAvailable = map toMisoString jnrls
  }

updateModel (SetFileName name) m@Model{..} = noEff m
  { currentJournal = (name, snd currentJournal)
  }

updateModel (ShowFileList b) m = noEff m { showingFileList = b }

updateModel (OpenFile name) m = m <# do
  rsRetrieve $ fromMisoString name
  pure NoOp

updateModel (SetUnparsedJournal uj) m@Model{..} = m
  { currentJournal = (fst currentJournal, uj)
  , waitingDebounced = waitingDebounced + 1
  , status = Just $ toMisoString $ Data.Text.concat
    [ "waiting to parse..."
    , Data.Text.pack $ show $ waitingDebounced + 1
    ]
  } <# do
    threadDelay 800000 -- 0.8s
    pure MaybeParseJournal

updateModel MaybeParseJournal m@Model{..} = m
  { waitingDebounced = waitingDebounced - 1
  } <# do
    if waitingDebounced == 1
      then pure ImmediatelyParseJournal
      else pure NoOp

updateModel ImmediatelyParseJournal m@Model{..} = m
  { status = Just $ pack "parsing journal..."
  } <# do
    ParsedJournal <$> parseJournal (snd currentJournal)

updateModel (ParsedJournal res) m = noEff updated
  where
    updated = case res of
      Left err -> m
        { err = Just $ pack err
        , parsedJournal = Nothing
        , status = Just $ pack "encountered error while parsing."
        }
      Right jrnl -> m
        { parsedJournal = Just jrnl
        , err = Nothing
        , status = Just $ pack "finished parsing!"
        }

updateModel SaveCurrent m@Model{..} = m
  { currentlySaved = currentJournal
  } <# do
    rsPut (fst currentJournal) (fromMisoString $ snd currentJournal)
    rsList
    pure NoOp

viewModel :: Model -> View Action
viewModel Model {..} = div_ []
  [ nav_ [ class_ "navbar" ]
    [ div_ [ class_ "navbar-brand" ]
      [ a_ [ class_ "navbar-item" ]
        [ h1_ [ class_ "title is-1" ] [ text $ pack "hledger interactive" ]
        ]
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
          ] [ text $ snd currentJournal ]
        , div_ [ class_ "columns save" ]
          [ div_ [ class_ "column" ]
            [ button_
              [ class_ "button is-info"
              , disabled_ $ not logged
              , onClick (ShowFileList True)
              ] [ text $ pack "Open" ]
            ]
          , div_ [ class_ "column" ]
            [ input_
              [ class_ "input"
              , value_ $ fst currentJournal
              , onInput SetFileName
              ] []
            ]
          , div_ [ class_ "column is-narrow" ]
            [ button_
              [ class_ "button is-primary"
              , disabled_ $ currentlySaved == currentJournal
              , onClick SaveCurrent
              ] [ text $ pack "Save" ]
            ]
          ]
        ]
      , div_ [ class_ "column" ]
        $ case parsedJournal of
          Nothing -> []
          Just jrnl ->
            [ div_ []
              [ h4_ [ class_ "title is-4" ] [ text $ pack "Account Balances" ]
              , viewAccountTree $ Prelude.tail $ accountTree jrnl
              ]
            , div_ [ class_ "transaction-list" ]
              [ h4_ [ class_ "title is-4" ] [ text $ pack "Transactions" ]
              , ul_ []
                $ map viewTransaction (jtxns jrnl)
              ]
            , div_ [ class_ "what" ]
              [ h4_ [ class_ "title is-4" ] [ text $ pack "What is this?" ]
              , p_ []
                [ a_
                  [ href_ "http://hledger.org/"
                  , target_ "_blank"
                  ] [ text $ pack "hledger" ]
                ]
              , p_ []
                [ a_
                  [ href_ "https://remotestorage.io/"
                  , target_ "_blank"
                  ] [ text $ pack "remoteStorage" ]
                ]
              , p_ []
                [ a_
                  [ href_ "https://github.com/fiatjaf/d"
                  , target_ "_blank"
                  ] [ text $ pack "source" ]
                ]
              , p_ []
                [ a_
                  [ href_ "https://fiatjaf.alhur.es"
                  , target_ "_blank"
                  ] [ text $ pack "author" ]
                ]
              ]
            ]
      ]
    ]
  , div_ [ classList_ [ ("modal", True), ("is-active", showingFileList) ] ]
    [ div_ [ class_ "modal-background", onClick (ShowFileList False) ] []
    , div_ [ class_ "modal-content" ]
      [ div_ [ class_ "box" ]
        [ h4_ [ class_ "title is-4" ] [ text $ pack "Files on remoteStorage" ]
        , ul_ []
          $ map
            (\jn -> li_ [] [ a_ [ onClick (OpenFile jn) ] [ text jn ]])
            journalsAvailable
        ]
      ]
    , div_ [ class_ "modal-close", onClick (ShowFileList False) ] []
    ]
  ]

viewTransaction :: Transaction -> View Action
viewTransaction txn =
  li_ []
    [ pre_ [] [ text $ toMisoString (showTransaction txn) ]
    ]

viewAccountTree :: [Account] -> View Action
viewAccountTree accounts =
  div_ [ class_ "account-tree" ] $ map viewAccount accounts

viewAccount :: Account -> View Action
viewAccount acc@Account{..} =
  let
    offset = (parentAccounts acc & Prelude.length) - 1
  in div_ []
    [ div_ [ class_ "columns" ]
      [ div_ [ class_ "column name" ]
        [ text $ toMisoString $ replicate offset "  "
        , text $ toMisoString $ accountLeafName aname
        ]
      , viewAmounts aebalance
      ]
    ]

viewAmounts :: MixedAmount -> View Action
viewAmounts (Mixed amts) =
  div_ [ class_ "column is-narrow" ] $ map viewAmount amts

viewAmount :: Amount -> View Action
viewAmount (Amount {..}) =
  div_ [ class_ "amount" ]
    [ text $ toMisoString acommodity
    , text $ pack " "
    , text $ pack $ show aquantity
    ]


foreign import javascript unsafe
  "onLogged($1)"
  onLogged :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "onGet($1)"
  onGet :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "retrieveFile($1)"
  rsRetrieve :: JSString -> IO ()

foreign import javascript unsafe
  "onList($1)"
  onList :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "listFiles()"
  rsList :: IO ()

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
      let path = textFromJSVal p
      let contents = textFromJSVal v
      sink $ GotValue (path, contents)

listSub :: Sub Action Model
listSub _ = \sink -> do
  onList =<< do
    asyncCallback1 $ \files -> do
      sink $ GotListing $ map textFromJSVal $ toList $ SomeJSArray files

loggedSub :: Sub Action Model
loggedSub _ = \sink -> do
  onLogged =<< do
    asyncCallback1 $ \logged -> sink $ GotLogged $ isTruthy logged
