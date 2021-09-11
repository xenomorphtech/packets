module Example.Driver.Websockets.Log where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import CSS.Overflow as Overflow 
import Halogen.HTML.CSS as CSS
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Simple.JSON as JSON
import Data.Either
import Data.Int
import Data.String as String

type Slot = H.Slot Query Message

data Query a = ReceiveMessage String a

data Message = OutputMessage String

data Action
  = HandleInput String
  | HandleInputSearch String
  | Submit Event
  | Select Int
  | TurnFilter
  | PauseResume 
  | Clear
  | RunSearch
  | ClearSearch

 
data EntryType 
  = Recv 
  | Send 
  | Log 
  | Other


type Entry =
  { full :: String
  , summary :: String
  , index :: Int
  , etype :: EntryType 
  }

type EntryJson =
  { full :: String
  , summary :: String
  , etype :: String 
  }

type Entries = Array Entry

type State =
  { messages :: Entries 
  , inputText :: String
  , selectedIndex :: Int
  , selectedText :: String 
  , paused :: Boolean
  , filter :: Boolean
  , currentWindowStart :: Int
  , searchResult :: Maybe Entries 
  , searchText :: String
  }

type DisplayEntry =
  { txt :: String
  , index :: Int 
  , etype :: EntryType 
  }


component :: forall i m. MonadEffect m => H.Component Query i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: forall i. i -> State
initialState _ = { messages: [] , 
                   inputText: "", 
                   selectedIndex: 0, 
                   selectedText: "" , 
                   paused: false,
                   filter: false,
                   searchResult: Nothing,
                   currentWindowStart: 0,
                   searchText: ""}

displayEntry :: String -> Int -> EntryType -> DisplayEntry
displayEntry a b etype = {txt: a, index: b, etype: etype}

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let len = A.length state.messages
      start = (max 0 (len - 5000))
      slice = A.slice start len state.messages
      msgs = case state.searchResult of
           Just msgs -> A.mapWithIndex (\index a -> displayEntry a.summary (index + start) a.etype) msgs 
           Nothing -> A.mapWithIndex (\index a -> displayEntry a.summary (index + start) a.etype) slice 
      pause_button = HH.button
                         [ HE.onClick (\_ -> PauseResume)] 
                         [ HH.text pause_text ]
                         where
                               pause_text = if state.paused == true 
                                              then "resume"
                                              else "pause"
      filter_button = HH.button
                         [ HE.onClick (\_ -> TurnFilter)] 
                         [ HH.text text ]
                         where
                               text = if state.filter == true 
                                              then "off filter"
                                              else "on filter"
                        
      clear_button = HH.button
                         [ HE.onClick (\_ -> Clear)] 
                         [ HH.text "clear" ]
            
      search_button = HH.button
                         [ HE.onClick (\_ -> RunSearch)] 
                         [ HH.text "search" ]

      search_clear_button = HH.button
                         [ HE.onClick (\_ -> ClearSearch)] 
                         [ HH.text "clear search" ]
 
      form = HH.form
               [ HE.onSubmit Submit ] 
               [
                 HH.input
                    [ HP.type_ HP.InputText
                    , HP.value (state.inputText)
                    , HE.onValueInput HandleInput
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonSubmit ]
                    [ HH.text "Send Message" ]
                , pause_button 
                , filter_button
                , clear_button
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.value (state.searchText)
                    , HE.onValueInput HandleInputSearch
                    ]
                , search_button
                , search_clear_button 
                ]

   in
       HH.div_ 
         [
          form
          , HH.div
             [  HP.class_ $ ClassName "float-container" ]
         [
          HH.div 
             [  HP.class_ $ ClassName "packets" ]
              ( map  (\msg -> HH.div [ 
                HP.class_ $ ClassName (entry_type msg.etype)
                , HE.onClick (\_ -> Select msg.index) 
                ] [ HH.text msg.txt ]) msgs) 
          , HH.div 
             [  HP.class_ $ ClassName "float-child" ]
             [ HH.textarea 
                    [ HP.value state.selectedText]
               ]
         ]
       ]
     

entry_type_json :: String -> EntryType 
entry_type_json etype = 
  case etype of
       "recv" -> Recv
       "send" -> Send
       "log" -> Log
       _ -> Other

entry_type :: EntryType -> String
entry_type etype = 
  case etype of
       Other -> "other"
       Recv -> "recv"
       Send -> "sentpacket"
       Log -> "log"



handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  HandleInput text -> do
    H.modify_ (_ { inputText = text })

  HandleInputSearch text -> do
    H.modify_ (_ { searchText = text })

  Submit ev -> do
    H.liftEffect $ Event.preventDefault ev
    st <- H.get
    let outgoingMessage = st.inputText
    H.raise $ OutputMessage outgoingMessage
    H.modify_ \st' -> st'
      {
       inputText = ""
      }

  RunSearch -> do
    st <- H.get
    H.modify_ \st' -> st'
      {
         searchResult = runSearch st' 
      }

  Select ev -> do
    H.modify_ \st' -> updateText st' ev

  PauseResume -> do
    H.modify_ (\st -> st{ paused = switch st.paused} ) 


  TurnFilter -> do
    H.modify_ (\st -> st{ filter = switch st.filter} ) 
    st <- H.get
    let onoff = "filter" <> case st.filter of
                                   true -> "on"
                                   false -> "off"
    H.raise $ OutputMessage onoff

  Clear -> do
    H.modify_ (\st -> st{ messages = [], currentWindowStart = 0, searchResult = Nothing} ) 

  ClearSearch -> do
    H.modify_ (\st -> st{  searchResult = Nothing} ) 


runSearch :: State -> Maybe Entries 
runSearch st =
  let searchtext = String.Pattern st.searchText
      result = A.filter (\a -> not $ (String.indexOf searchtext a.full) == Nothing ) st.messages
  in Just result 



switch :: Boolean -> Boolean
switch true = false 
switch false = true                  

updateText :: State -> Int -> State
updateText st ev = 
      let messagesSource = case st.searchResult of
                                Nothing -> st.messages
                                (Just res) -> res 
          something = case (A.index messagesSource ev) of
                           (Just x) -> x.full
                           Nothing -> "(NULL)"
       in
        st {
          selectedText = something
        }
     


append_msg :: String -> State -> State
append_msg incomingMessage st = 
  case JSON.readJSON incomingMessage of
    Right (r :: EntryJson) -> do  
         st { messages = dropcond `A.snoc` nentry }
         where
               messages = st.messages
               dropcond = if A.length st.messages > 50000 
                          then A.drop 1 st.messages
                          else st.messages
               nentry = new_entry r 
    Left e -> 
      st 


new_entry :: EntryJson -> Entry
new_entry r = 
  { summary : r.summary, full: r.full, index: 0, etype: (entry_type_json r.etype)} 

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  ReceiveMessage msg a -> do
    let incomingMessage = " " <> msg
    H.modify_ (\st -> append_msg incomingMessage st )
    pure (Just a)
