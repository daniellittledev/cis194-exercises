module Week02.LogAnalysis
  (
    readUntil,
    parseMessage,
    parse,
    insert,
    build,
    inOrder,
    whatWentWrong,
    module Week02.Log
  )
where

import           Week02.Log                     ( LogMessage(..)
                                                , MessageTree(..)
                                                , MessageType(..)
                                                , TimeStamp
                                                , testWhatWentWrong
                                                , testParse
                                                )

readUntil :: (a -> Bool) -> [a] -> ([a], [a])
readUntil _ [] = ([], [])
readUntil p (h:xs) =
  if not $ p h then
    let (x, y) = readUntil p xs in
    (h : x, y)
  else
    ([], xs)

parseIntAndStr :: String -> (Int, String)
parseIntAndStr s =
  let (num, r) = readUntil (\ x -> x == ' ') s in
    (read num, r)

applyTuple :: (a -> b -> c) -> (a, b) -> c
applyTuple fn (x, y) = fn x y

parseMessage :: String -> LogMessage
parseMessage line =
  let (t, r) = readUntil (\ x -> x == ' ') line in
  case t of
    "I" -> applyTuple (LogMessage Info) (parseIntAndStr r)
    "W" -> applyTuple (LogMessage Warning) (parseIntAndStr r)
    "E" ->
      let (e, m) = parseIntAndStr r in
      applyTuple (LogMessage (Error e)) (parseIntAndStr m)
    _ -> Unknown line

  --let type = takeWhile (/x -> x /= " ")

parse :: String -> [LogMessage]
parse = error "Week02.LogAnalysis#parse not implemented"

insert :: LogMessage -> MessageTree -> MessageTree
insert = error "Week02.LogAnalysis#insert not implemented"

build :: [LogMessage] -> MessageTree
build = error "Week02.LogAnalysis#build not implemented"

inOrder :: MessageTree -> [LogMessage]
inOrder = error "Week02.LogAnalysis#inOrder not implemented"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = error "Week02.LogAnalysis#whatWentWrong not implemented"
