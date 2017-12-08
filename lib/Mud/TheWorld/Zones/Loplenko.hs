{-# LANGUAGE MonadComprehensions, NamedFieldPuns, OverloadedStrings, RecordWildCards #-}

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
import           Data.List ((\\))
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
                , (lookSundialHookName,         lookSundialHookFun        )
                , (readBookCreationHookName,    readBookCreationHookFun   )
                , (readBookDwarfHookName,       readBookDwarfHookFun      )
                , (readBookElfHookName,         readBookElfHookFun        )
                , (readBookFelinoidHookName,    readBookFelinoidHookFun   )
                , (readBookHistoryHookName,     readBookHistoryHookFun    )
                , (readBookHobbitHookName,      readBookHobbitHookFun     )
                , (readBookHolyHookName,        readBookHolyHookFun       )
                , (readBookHumanHookName,       readBookHumanHookFun      )
                , (readBookLagomorphHookName,   readBookLagomorphHookFun  )
                , (readBookLopolwanmiHookName,  readBookLopolwanmiHookFun )
                , (readBookMapsHookName,        readBookMapsHookFun       )
                , (readBookNymphHookName,       readBookNymphHookFun      )
                , (readBookRacesHookName,       readBookRacesHookFun      )
                , (readBookShunfalipmiHookName, readBookShunfalipmiHookFun)
                , (readBookVulpenoidHookName,   readBookVulpenoidHookFun  )
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
                    in pure (serialize selfDesig <> " peruses the books on the bookshelves.", desigOtherIds selfDesig) )
      & _2._4 <>~ pure (bracketQuote hookName <> " bookshelves")
      & _3    <>~ pure helper
  where
    helper       = pager i mq Nothing =<< eitherRet handler =<< try readBookList
    readBookList = [ map xformLeadingSpaceChars . parseWrap s cols $ cont | file <- liftIO . mkMudFilePath $ bookListFileFun
                                                                          , cont <- liftIO . T.readFile $ file
                                                                          , s    <- getServerSettings ]
    handler e    = fileIOExHandler "lookBookshelvesHookFun" e >> return (wrap cols bookListErrorMsg)
    (mq, cols)   = getMsgQueueColumns i ms

-----

lookSundialHook :: Hook
lookSundialHook = Hook lookSundialHookName [ "sundial", "dial", "sun" ]

lookSundialHookName :: HookName
lookSundialHookName = "Loplenko_iLoplenkoWelcome_lookSundial"

lookSundialHookFun :: HookFun
lookSundialHookFun = mkGenericHookFun t "looks at the sundial." "looked at sundial"
  where
    t = T.concat [ "The sundial is inlaid into a 3-foot-high square base of white marble. A triangle affixed \
                   \perpendicular to the sundial's flat plate casts a shadow when the sun's light hits it; the time can \
                   \be determined based on where that shadow falls on the plate."
                 , nlTxt, "You can ", dblQuote "read", " the sundial to tell the time." ]

-----

readBookHelper :: Book -> HookFun
readBookHelper b i Hook { .. } _ a@(_, (ms, _, _, _), _) =
    a & _1    %~  (\\ hookTriggers)
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure (serialize selfDesig <> " reads a book.", desigOtherIds selfDesig) )
      & _2._4 <>~ pure (T.concat [ bracketQuote hookName, " ", dblQuote . pp $ b, " book" ])
      & _3    <>~ pure (readABook i b)

readABook :: Id -> Book -> MudStack ()
readABook i b = ((,) <$> getState <*> getServerSettings) >>= \(ms, s) ->
    let (mq, cols)     = getMsgQueueColumns i ms
        rmDescHelper x = tweak $ mobTbl.ind i.mobRmDesc .~ x
        next           = rmDescHelper Nothing >> setInterp i Nothing >> sendDfltPrompt mq i >> bcastNl bs
        bs             = pure (serialize selfDesig <> " finishes reading a book.", desigOtherIds selfDesig)
        selfDesig      = mkStdDesig i ms DoCap
    in do rmDescHelper . Just $ "reading a book"
          pager i mq (Just next) =<< parseBookTxt s cols <$> getBookTxt b cols

parseBookTxt :: ServerSettings -> Cols -> Text -> [Text]
parseBookTxt s cols = map (xformLeadingSpaceChars . expandDividers cols) . parseWrap s cols

getBookTxt :: Book -> Cols -> MudStack Text
getBookTxt b cols = liftIO (T.readFile =<< mkFilePath) |&| try >=> eitherRet handler
  where
    mkFilePath = mkMudFilePath $ case b of BookCreation    -> bookCreationFileFun
                                           BookDwarf       -> bookDwarfFileFun
                                           BookElf         -> bookElfFileFun
                                           BookFelinoid    -> bookFelinoidFileFun
                                           BookHistory     -> bookHistoryFileFun
                                           BookHobbit      -> bookHobbitFileFun
                                           BookHoly        -> bookHolyFileFun
                                           BookHuman       -> bookHumanFileFun
                                           BookLagomorph   -> bookLagomorphFileFun
                                           BookLopolwanmi  -> bookLopoLwanmiFileFun
                                           BookMaps        -> bookMapsFileFun
                                           BookNymph       -> bookNymphFileFun
                                           BookRaces       -> bookRacesFileFun
                                           BookShunfalipmi -> bookShunfalipmiFileFun
                                           BookVulpenoid   -> bookVulpenoidFileFun
    handler e = do fileIOExHandler "getBookTxt" e
                   return . wrapUnlines cols . bookFileErrorMsg . dblQuote . pp $ b

-----

readBookCreationHook :: Hook
readBookCreationHook = Hook readBookCreationHookName . pure $ "creation"

readBookCreationHookName :: HookName
readBookCreationHookName = "Loplenko_iLibrary_readBookCreation"

readBookCreationHookFun :: HookFun
readBookCreationHookFun = readBookHelper BookCreation

-----

readBookDwarfHook :: Hook
readBookDwarfHook = Hook readBookDwarfHookName . pure $ "dwarf"

readBookDwarfHookName :: HookName
readBookDwarfHookName = "Loplenko_iLibrary_readBookDwarf"

readBookDwarfHookFun :: HookFun
readBookDwarfHookFun = readBookHelper BookDwarf

-----

readBookElfHook :: Hook
readBookElfHook = Hook readBookElfHookName . pure $ "elf"

readBookElfHookName :: HookName
readBookElfHookName = "Loplenko_iLibrary_readBookElf"

readBookElfHookFun :: HookFun
readBookElfHookFun = readBookHelper BookElf

-----

readBookFelinoidHook :: Hook
readBookFelinoidHook = Hook readBookFelinoidHookName . pure $ "felinoid"

readBookFelinoidHookName :: HookName
readBookFelinoidHookName = "Loplenko_iLibrary_readBookFelinoid"

readBookFelinoidHookFun :: HookFun
readBookFelinoidHookFun = readBookHelper BookFelinoid

-----

readBookHistoryHook :: Hook
readBookHistoryHook = Hook readBookHistoryHookName . pure $ "history"

readBookHistoryHookName :: HookName
readBookHistoryHookName = "Loplenko_iLibrary_readBookHistory"

readBookHistoryHookFun :: HookFun
readBookHistoryHookFun = readBookHelper BookHistory

-----

readBookHobbitHook :: Hook
readBookHobbitHook = Hook readBookHobbitHookName . pure $ "hobbit"

readBookHobbitHookName :: HookName
readBookHobbitHookName = "Loplenko_iLibrary_readBookHobbit"

readBookHobbitHookFun :: HookFun
readBookHobbitHookFun = readBookHelper BookHobbit

-----

readBookHolyHook :: Hook
readBookHolyHook = Hook readBookHolyHookName . pure $ "holy"

readBookHolyHookName :: HookName
readBookHolyHookName = "Loplenko_iLibrary_readBookHoly"

readBookHolyHookFun :: HookFun
readBookHolyHookFun = readBookHelper BookHoly

-----

readBookHumanHook :: Hook
readBookHumanHook = Hook readBookHumanHookName . pure $ "human"

readBookHumanHookName :: HookName
readBookHumanHookName = "Loplenko_iLibrary_readBookHuman"

readBookHumanHookFun :: HookFun
readBookHumanHookFun = readBookHelper BookHuman

-----

readBookLagomorphHook :: Hook
readBookLagomorphHook = Hook readBookLagomorphHookName . pure $ "lagomorph"

readBookLagomorphHookName :: HookName
readBookLagomorphHookName = "Loplenko_iLibrary_readBookLagomorph"

readBookLagomorphHookFun :: HookFun
readBookLagomorphHookFun = readBookHelper BookLagomorph

-----

readBookLopolwanmiHook :: Hook
readBookLopolwanmiHook = Hook readBookLopolwanmiHookName . pure $ "lopolwanmi"

readBookLopolwanmiHookName :: HookName
readBookLopolwanmiHookName = "Loplenko_iLibrary_readBookLopolwanmi"

readBookLopolwanmiHookFun :: HookFun
readBookLopolwanmiHookFun = readBookHelper BookLopolwanmi

-----

readBookMapsHook :: Hook
readBookMapsHook = Hook readBookMapsHookName . pure $ "maps"

readBookMapsHookName :: HookName
readBookMapsHookName = "Loplenko_iLibrary_readBookMaps"

readBookMapsHookFun :: HookFun
readBookMapsHookFun = readBookHelper BookMaps

-----

readBookNymphHook :: Hook
readBookNymphHook = Hook readBookNymphHookName . pure $ "nymph"

readBookNymphHookName :: HookName
readBookNymphHookName = "Loplenko_iLibrary_readBookNymph"

readBookNymphHookFun :: HookFun
readBookNymphHookFun = readBookHelper BookNymph

-----

readBookRacesHook :: Hook
readBookRacesHook = Hook readBookRacesHookName . pure $ "races"

readBookRacesHookName :: HookName
readBookRacesHookName = "Loplenko_iLibrary_readBookRaces"

readBookRacesHookFun :: HookFun
readBookRacesHookFun = readBookHelper BookRaces

-----

readBookShunfalipmiHook :: Hook
readBookShunfalipmiHook = Hook readBookShunfalipmiHookName . pure $ "shunfalipmi"

readBookShunfalipmiHookName :: HookName
readBookShunfalipmiHookName = "Loplenko_iLibrary_readBookShunfalipmi"

readBookShunfalipmiHookFun :: HookFun
readBookShunfalipmiHookFun = readBookHelper BookShunfalipmi

-----

readBookVulpenoidHook :: Hook
readBookVulpenoidHook = Hook readBookVulpenoidHookName . pure $ "vulpenoid"

readBookVulpenoidHookName :: HookName
readBookVulpenoidHookName = "Loplenko_iLibrary_readBookVulpenoid"

readBookVulpenoidHookFun :: HookFun
readBookVulpenoidHookFun = readBookHelper BookVulpenoid

-----

readSundialHook :: Hook
readSundialHook = Hook readSundialHookName [ "sundial", "dial", "sun" ]

readSundialHookName :: HookName
readSundialHookName = "Loplenko_iLoplenkoWelcome_readSundial"

readSundialHookFun :: HookFun
readSundialHookFun i Hook { .. } _ a@(_, (ms, _, _, _), _) =
    a & _1    %~  (\\ hookTriggers)
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure (serialize selfDesig <> " reads the sundial.", desigOtherIds selfDesig) )
      & _2._4 <>~ pure (bracketQuote hookName <> " read sundial")
      & _3    .~  pure (uncurry helper . getMsgQueueColumns i $ ms)
  where
    helper mq cols = liftIO getCurryTime >>= \CurryTime { .. } -> wrapSend mq cols $ if isDay curryHour
      then T.concat [ "The sundial reads ", showTxt curryHour, ":", padTwoDigits $ (curryMin `div` 5) * 5, "." ]
      else "Alas, you'll have to wait for the sun to come out."

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
            \There is a sundial here.\n\
            \There is a trash bin here."
            Nothing
            Nothing
            zeroBits
            []
            (0, 0, 0)
            OutsideEnv
            (Just "Welcome")
            (M.fromList [ ("look", [ lookSundialHook, lookTrashHook ])
                        , ("put",  [ putTrashHook                   ])
                        , ("read", [ readSundialHook                ]) ])
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
            InsideLitEnv
            (Just "Library")
            (M.fromList [ ("look", [ lookBookshelvesHook, lookTrashHook ])
                        , ("put",  [ putTrashHook                       ])
                        , ("read", [ readBookCreationHook
                                   , readBookDwarfHook
                                   , readBookElfHook
                                   , readBookFelinoidHook
                                   , readBookHistoryHook
                                   , readBookHobbitHook
                                   , readBookHolyHook
                                   , readBookHumanHook
                                   , readBookLagomorphHook
                                   , readBookLopolwanmiHook
                                   , readBookMapsHook
                                   , readBookNymphHook
                                   , readBookRacesHook
                                   , readBookShunfalipmiHook
                                   , readBookVulpenoidHook ]) ])
            []
            []))

  putRmTeleName iLoplenkoWelcome "loplenko"
  putRmTeleName iLibrary         "library"
