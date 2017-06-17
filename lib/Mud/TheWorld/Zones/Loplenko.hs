{-# LANGUAGE MonadComprehensions, OverloadedStrings, RecordWildCards #-}

module Mud.TheWorld.Zones.Loplenko ( createLoplenko
                                   , loplenkoHooks
                                   , loplenkoRmActionFuns ) where

import           Mud.Cmds.Msgs.Misc
import           Mud.Cmds.Util.Misc
import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Data.State.Util.Put
import           Mud.Misc.CurryTime
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.Misc.Misc
import           Mud.TheWorld.Misc
import           Mud.TheWorld.Zones.LoplenkoIds
import           Mud.Threads.Misc
import           Mud.TopLvlDefs.FilePaths
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Padding
import           Mud.Util.Quoting
import           Mud.Util.Text
import           Mud.Util.Wrapping

import           Control.Exception.Lifted (try)
import           Control.Lens (_1, _2, _3, _4)
import           Control.Lens.Operators ((.~), (&), (%~), (<>~))
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Bits (zeroBits)
import           Data.List ((\\), delete)
import qualified Data.Map.Strict as M (fromList)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Zones.Loplenko"


-- ==================================================
-- Hooks:


loplenkoHooks :: [(HookName, HookFun)]
loplenkoHooks = [ (lookBookshelvesHookName,     lookBookshelvesHookFun    )
                , (lookMoondialHookName,        lookMoondialHookFun       )
                , (lookSundialHookName,         lookSundialHookFun        )
                , (readBookCreationHookName,    readBookCreationHookFun   )
                , (readBookDwarfHookName,       readBookDwarfHookFun      )
                , (readBookElfHookName,         readBookElfHookFun        )
                , (readBookFelinoidHookName,    readBookFelinoidHookFun   )
                , (readBookHobbitHookName,      readBookHobbitHookFun     )
                , (readBookHolyHookName,        readBookHolyHookFun       )
                , (readBookHumanHookName,       readBookHumanHookFun      )
                , (readBookLagomorphHookName,   readBookLagomorphHookFun  )
                , (readBookLopolwanmiHookName,  readBookLopolwanmiHookFun )
                , (readBookNymphHookName,       readBookNymphHookFun      )
                , (readBookRacesHookName,       readBookRacesHookFun      )
                , (readBookRumiaHookName,       readBookRumiaHookFun      )
                , (readBookShunfalipmiHookName, readBookShunfalipmiHookFun)
                , (readBookVulpenoidHookName,   readBookVulpenoidHookFun  )
                , (readMoondialHookName,        readMoondialHookFun       )
                , (readSundialHookName,         readSundialHookFun        ) ]


-----


lookBookshelvesHook :: Hook
lookBookshelvesHook = Hook lookBookshelvesHookName [ "book", "books", "bookshelf", "bookshelves", "shelf", "shelves" ]


lookBookshelvesHookName :: HookName
lookBookshelvesHookName = "Loplenko_iLibrary_lookBookshelves"


lookBookshelvesHookFun :: HookFun
lookBookshelvesHookFun i Hook { .. } _ a@(_, (ms, _, _, _), _) =
    a & _1    %~  (\\ hookTriggers)
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure ( serialize selfDesig <> " peruses the books on the bookshelves."
                            , i `delete` desigIds selfDesig ) )
      & _2._4 <>~ pure (bracketQuote hookName <> " bookshelves")
      & _3    <>~ pure (send mq =<< helper)
  where
    helper       = liftIO readBookList |&| try >=> eitherRet handler
    readBookList = [ multiWrap cols . T.lines $ cont | file <- mkMudFilePath bookListFileFun
                                                     , cont <- T.readFile file ]
    handler e    = fileIOExHandler "lookBookshelvesHookFun" e >> return (wrapUnlinesNl cols bookListErrorMsg)
    (mq, cols)   = getMsgQueueColumns i ms


-----


lookMoondialHook :: Hook
lookMoondialHook = Hook lookMoondialHookName . pure $ "moondial"


lookMoondialHookName :: HookName
lookMoondialHookName = "Loplenko_iLoplenkoWelcome_lookMoondial"


lookMoondialHookFun :: HookFun
lookMoondialHookFun = mkGenericHookFun (mkDialDesc "moon") "looks at the moondial." "looked at moondial"


mkDialDesc :: Text -> Text
mkDialDesc t = T.concat [ "The "
                        , t
                        , "dial is inlaid into a 3-foot-high square base of white marble. Two triangles affixed \
                          \perpendicular to the "
                        , t
                        , "dial's flat plate cast a shadow when the "
                        , t
                        , "'s light hits them; the time can be determined based on where that shadow falls on the \
                          \plate."
                        , nlTxt
                        , "You can "
                        , dblQuote "read"
                        , " the "
                        , t
                        , "dial to tell the time." ]


-----


lookSundialHook :: Hook
lookSundialHook = Hook lookSundialHookName . pure $ "sundial"


lookSundialHookName :: HookName
lookSundialHookName = "Loplenko_iLoplenkoWelcome_lookSundial"


lookSundialHookFun :: HookFun
lookSundialHookFun = mkGenericHookFun (mkDialDesc "sun") "looks at the sundial." "looked at sundial"


-----


readBookHelper :: Book -> HookFun
readBookHelper b i Hook { .. } _ a@(_, (ms, _, _, _), _) =
    a & _1    %~  (\\ hookTriggers)
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure ( serialize selfDesig <> " reads a book."
                            , i `delete` desigIds selfDesig ) )
      & _2._4 <>~ pure (T.concat [ bracketQuote hookName, " ", dblQuote . pp $ b, " book" ])
      & _3    <>~ pure (readABook i b)


readABook :: Id -> Book -> MudStack () -- TODO: Set and unset room desc.
readABook i b = ((,) <$> getState <*> getServerSettings) >>= \(ms, s) ->
    let (mq, cols) = getMsgQueueColumns i ms
    in pager i mq Nothing =<< parseBookTxt s cols <$> getBookTxt b cols


parseBookTxt :: ServerSettings -> Cols -> Text -> [Text]
parseBookTxt s cols = map (xformLeadingSpaceChars . expandDividers cols) . parseWrap s cols


getBookTxt :: Book -> Cols -> MudStack Text
getBookTxt b cols = liftIO (T.readFile =<< mkFilePath) |&| try >=> eitherRet handler
  where
    mkFilePath = mkMudFilePath $ case b of BookCreation    -> bookCreationFileFun
                                           BookDwarf       -> bookDwarfFileFun
                                           BookElf         -> bookElfFileFun
                                           BookFelinoid    -> bookFelinoidFileFun
                                           BookHobbit      -> bookHobbitFileFun
                                           BookHoly        -> bookHolyFileFun
                                           BookHuman       -> bookHumanFileFun
                                           BookLagomorph   -> bookLagomorphFileFun
                                           BookLopolwanmi  -> bookLopoLwanmiFileFun
                                           BookNymph       -> bookNymphFileFun
                                           BookRaces       -> bookRacesFileFun
                                           BookRumia       -> bookRumiaFileFun
                                           BookShunfalipmi -> bookShunfalipmiFileFun
                                           BookVulpenoid   -> bookVulpenoidFileFun
    handler e = do fileIOExHandler "getBookTxtByName" e
                   return . wrapUnlines cols . bookFileErrorMsg . dblQuote . pp $ b


-----


readBookCreationHook :: Hook
readBookCreationHook = Hook readBookCreationHookName ["creation"]


readBookCreationHookName :: HookName
readBookCreationHookName = "Loplenko_iLibrary_readBookCreation"


readBookCreationHookFun :: HookFun
readBookCreationHookFun = readBookHelper BookCreation


-----


readBookDwarfHook :: Hook
readBookDwarfHook = Hook readBookDwarfHookName ["dwarf"]


readBookDwarfHookName :: HookName
readBookDwarfHookName = "Loplenko_iLibrary_readBookDwarf"


readBookDwarfHookFun :: HookFun
readBookDwarfHookFun = readBookHelper BookDwarf


-----


readBookElfHook :: Hook
readBookElfHook = Hook readBookElfHookName ["elf"]


readBookElfHookName :: HookName
readBookElfHookName = "Loplenko_iLibrary_readBookElf"


readBookElfHookFun :: HookFun
readBookElfHookFun = readBookHelper BookElf


-----


readBookFelinoidHook :: Hook
readBookFelinoidHook = Hook readBookFelinoidHookName ["felinoid"]


readBookFelinoidHookName :: HookName
readBookFelinoidHookName = "Loplenko_iLibrary_readBookFelinoid"


readBookFelinoidHookFun :: HookFun
readBookFelinoidHookFun = readBookHelper BookFelinoid


-----


readBookHobbitHook :: Hook
readBookHobbitHook = Hook readBookHobbitHookName ["hobbit"]


readBookHobbitHookName :: HookName
readBookHobbitHookName = "Loplenko_iLibrary_readBookHobbit"


readBookHobbitHookFun :: HookFun
readBookHobbitHookFun = readBookHelper BookHobbit


-----


readBookHolyHook :: Hook
readBookHolyHook = Hook readBookHolyHookName ["holy"]


readBookHolyHookName :: HookName
readBookHolyHookName = "Loplenko_iLibrary_readBookHoly"


readBookHolyHookFun :: HookFun
readBookHolyHookFun = readBookHelper BookHoly


-----


readBookHumanHook :: Hook
readBookHumanHook = Hook readBookHumanHookName ["human"]


readBookHumanHookName :: HookName
readBookHumanHookName = "Loplenko_iLibrary_readBookHuman"


readBookHumanHookFun :: HookFun
readBookHumanHookFun = readBookHelper BookHuman


-----


readBookLagomorphHook :: Hook
readBookLagomorphHook = Hook readBookLagomorphHookName ["lagomorph"]


readBookLagomorphHookName :: HookName
readBookLagomorphHookName = "Loplenko_iLibrary_readBookLagomorph"


readBookLagomorphHookFun :: HookFun
readBookLagomorphHookFun = readBookHelper BookLagomorph


-----


readBookLopolwanmiHook :: Hook
readBookLopolwanmiHook = Hook readBookLopolwanmiHookName ["lopolwanmi"]


readBookLopolwanmiHookName :: HookName
readBookLopolwanmiHookName = "Loplenko_iLibrary_readBookLopolwanmi"


readBookLopolwanmiHookFun :: HookFun
readBookLopolwanmiHookFun = readBookHelper BookLopolwanmi


-----


readBookNymphHook :: Hook
readBookNymphHook = Hook readBookNymphHookName ["nymph"]


readBookNymphHookName :: HookName
readBookNymphHookName = "Loplenko_iLibrary_readBookNymph"


readBookNymphHookFun :: HookFun
readBookNymphHookFun = readBookHelper BookNymph


-----


readBookRacesHook :: Hook
readBookRacesHook = Hook readBookRacesHookName ["races"]


readBookRacesHookName :: HookName
readBookRacesHookName = "Loplenko_iLibrary_readBookRaces"


readBookRacesHookFun :: HookFun
readBookRacesHookFun = readBookHelper BookRaces


-----


readBookRumiaHook :: Hook
readBookRumiaHook = Hook readBookRumiaHookName ["rumia"]


readBookRumiaHookName :: HookName
readBookRumiaHookName = "Loplenko_iLibrary_readBookRumia"


readBookRumiaHookFun :: HookFun
readBookRumiaHookFun = readBookHelper BookRumia


-----


readBookShunfalipmiHook :: Hook
readBookShunfalipmiHook = Hook readBookShunfalipmiHookName ["shunfalipmi"]


readBookShunfalipmiHookName :: HookName
readBookShunfalipmiHookName = "Loplenko_iLibrary_readBookShunfalipmi"


readBookShunfalipmiHookFun :: HookFun
readBookShunfalipmiHookFun = readBookHelper BookShunfalipmi


-----


readBookVulpenoidHook :: Hook
readBookVulpenoidHook = Hook readBookVulpenoidHookName ["vulpenoid"]


readBookVulpenoidHookName :: HookName
readBookVulpenoidHookName = "Loplenko_iLibrary_readBookVulpenoid"


readBookVulpenoidHookFun :: HookFun
readBookVulpenoidHookFun = readBookHelper BookVulpenoid


-----


readMoondialHook :: Hook
readMoondialHook = Hook readMoondialHookName . pure $ "moondial"


readMoondialHookName :: HookName
readMoondialHookName = "Loplenko_iLoplenkoWelcome_readMoondial"


readMoondialHookFun :: HookFun
readMoondialHookFun i Hook { .. } _ a@(_, (ms, _, _, _), _) =
    a & _1    %~  (\\ hookTriggers)
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure (serialize selfDesig <> " reads the moondial.", i `delete` desigIds selfDesig) )
      & _2._4 <>~ pure (bracketQuote hookName <> " read moondial")
      & _3    .~  pure (uncurry helper . getMsgQueueColumns i $ ms)
  where
    helper mq cols = liftIO getCurryTime >>= \CurryTime { .. } ->
        let f msg = isNight curryHour ? msg :? "Alas, you'll have to wait for the moon to come out."
        in wrapSend mq cols . f $ case getMoonPhaseForDayOfMonth curryDayOfMonth of
          Just NewMoon -> "On account of the moon being absent from the sky tonight, you can't take a reading off the \
                          \moondial."
          _            -> T.concat [ "The moondial reads ", showTxt curryHour, ":", formatMins curryMin, "." ]


formatMins :: Min -> Text
formatMins x = padTwoDigits $ (x `div` 5) * 5


-----


readSundialHook :: Hook
readSundialHook = Hook readSundialHookName . pure $ "sundial"


readSundialHookName :: HookName
readSundialHookName = "Loplenko_iLoplenkoWelcome_readSundial"


readSundialHookFun :: HookFun
readSundialHookFun i Hook { .. } _ a@(_, (ms, _, _, _), _) =
    a & _1    %~  (\\ hookTriggers)
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure (serialize selfDesig <> " reads the sundial.", i `delete` desigIds selfDesig) )
      & _2._4 <>~ pure (bracketQuote hookName <> " read sundial")
      & _3    .~  pure helper
  where
    helper = do (ms', CurryTime { .. }) <- (,) <$> getState <*> liftIO getCurryTime
                let (mq, cols) = getMsgQueueColumns i ms'
                wrapSend mq cols $ if isNight curryHour
                  then "Alas, you'll have to wait for the sun to come out."
                  else T.concat [ "The sundial reads ", showTxt curryHour, ":", formatMins curryMin, "." ]


-- ==================================================
-- Room action functions:


loplenkoRmActionFuns :: [(FunName, RmActionFun)]
loplenkoRmActionFuns = []


-- ==================================================
-- Zone definition:


createLoplenko :: MudStack ()
createLoplenko = do
  logNotice "createLoplenko" "creating Lop'len-ko."

  -- ==================================================
  -- Rooms:
  putRm iLoplenkoWelcome
        []
        mempty
        (mkRm (RmTemplate "Welcome to Lop'len-ko"
            "Hello!\n\
            \A few feet from a sundial stands a similar moondial.\n\
            \There is a trash bin here."
            Nothing
            Nothing
            zeroBits
            []
            (0, 0, 0)
            OutsideEnv
            (Just "Welcome")
            (M.fromList [ ("look", [ lookSundialHook, lookMoondialHook, lookTrashHook ])
                        , ("put",  [ putTrashHook                                     ])
                        , ("read", [ readSundialHook, readMoondialHook                ]) ])
            [ trashRmAction ]
            []))

  putRm iLibrary
        []
        mempty
        (mkRm (RmTemplate "Library"
            "This is the library. There are a number of bookshelves.\n\
            \There is a trash bin here."
            Nothing
            Nothing
            zeroBits
            []
            (0, 0, -1)
            InsideEnv
            (Just "Library")
            (M.fromList [ ("look", [ lookBookshelvesHook ])
                        , ("put",  [ putTrashHook        ])
                        , ("read", [ readBookCreationHook
                                   , readBookDwarfHook
                                   , readBookElfHook
                                   , readBookFelinoidHook
                                   , readBookHobbitHook
                                   , readBookHolyHook
                                   , readBookHumanHook
                                   , readBookLagomorphHook
                                   , readBookLopolwanmiHook
                                   , readBookNymphHook
                                   , readBookRacesHook
                                   , readBookRumiaHook
                                   , readBookShunfalipmiHook
                                   , readBookVulpenoidHook ]) ])
            []
            []))

  putRmTeleName iLoplenkoWelcome "loplenko"
  putRmTeleName iLibrary         "library"
