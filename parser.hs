-- ghc parser.hs && .\parser.exe input.txt

import System.Environment (getArgs)
import System.IO
import Data.List

{-# LANGUAGE ImplicitParams    #-}
import GHC.Stack

-- https://www.parsonsmatt.org/2017/07/29/using_ghc_callstacks.html
-- https://donsbot.wordpress.com/2007/11/14/no-more-exceptions-debugging-haskell-code-with-ghci/
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Stack.html
head'       :: HasCallStack => [a] -> a
head' (x:_) =  x
head' []    =  error $ "head': empty list" ++ "\nCallStack: " ++ show callStack

main :: IO()
main = do
  args <- getArgs
  file <- openFile (head' args) ReadMode
  hSetEncoding file utf8
  text <- hGetContents file

  outh <- openFile ((getDir . head' $ args) ++ "result.txt") WriteMode
  hSetEncoding outh utf8

  let modified = convert . groupByDay . mfilter . lines $ text
    in hPutStrLn outh (unlines . unlines' $ modified)

  hClose file
  hClose outh


unlines' :: [[String]] -> [String]
unlines' [] = []
unlines' (x:xs) = x ++ unlines' xs

mfilter :: [String] -> [String]
mfilter lineList = filterTransfer . filterEvent . filterNull $ lineList

getDir :: String -> String
getDir str
  | length dirs > 1 = compl '\\' (init dirs)
  | otherwise = ""
  where dirs = wordsWhen (=='\\') str


-- 补回 delimter
compl :: Char -> [String] -> String
compl _ [] = []
compl delimter (x:xs) = x ++ [delimter] ++ compl delimter xs


-- 根据指定符号分割 String
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


-- 去除有转账记录的行
filterTransfer :: [String] -> [String]
filterTransfer [] = []
filterTransfer lineList = filter (\line -> not (isInfixOf "补助流水" line || isInfixOf "银行转账" line) ) lineList

filterNull :: [String] -> [String]
filterNull [] = []
filterNull lineList = filter (\line -> length line > 0) lineList

filterEvent :: [String] -> [String]
filterEvent lineList = filter (\line -> isInfixOf "/" (getDate line)) lineList

-- 将文本按日期划分
groupByDay :: [String] -> [[String]]
groupByDay = groupBy (\fst' snd' -> let fstDay = getDate fst'
                                        sndDay = getDate snd'
                                    in fstDay == sndDay)


-- 短时间内的交易视为同一个交易
-- 浴池：1h内都是一次洗澡
-- 吃饭：1h 内的打卡都是一次吃饭
convert :: [[String]] -> [[String]]
convert [] = []
convert (lineByDayList:linesList) = (convert' (groupByEventTime lineByDayList 1)):(convert linesList)

convert' :: [[String]] -> [String]
convert' [] = []
convert' (events:eventsList) = [combine events] ++ convert' eventsList

-- events 是同一天内相同的事件
combine :: [String] -> String
combine [] = ""
combine events = date ++ " * " ++ "\"" ++ time ++ "\" " ++ getEventsName events ++ "\n" ++ getDetail events
  where timeList = splitEvent (getDate . head' $ events) '/'
        complete numberString
          | length numberString < 2 = "0" ++ numberString
          | otherwise = numberString
        date = timeList !! 0 ++ "-" ++ complete (timeList !! 1) ++ "-" ++ complete (timeList !! 2)
        time = (getTimeList . head' $ events) !! 0 ++ ":" ++ (getTimeList . head' $ events) !! 1

remove_dups :: (Ord a, Eq a) => [a] -> [a]
remove_dups xs = remove $ sort xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)

flatten :: [String] -> String
flatten [] = []
flatten (event:events) = "\"" ++ event ++ "\" " ++ flatten events

getEventsName :: [String] -> String
getEventsName lineList = flatten . remove_dups $ (map getEvent lineList)

-- events 是同一天内相同的事件
getDetail :: [String] -> String
getDetail [] = ""
getDetail events
  | isInfixOf "浴池" (head' events) = "  Expenses:Housing:Bath +" ++ cost ++ " CNY\n" ++
                                      "  Assets:CampusCard:JLU -" ++ cost ++ " CNY\n\n"
  | otherwise  = "#" ++ (guessTime . head' $ events) ++
                 getDetailEvents events ++
                 "\n  Assets:CampusCard:JLU -" ++ cost ++ " CNY\n\n"
  where cost = (show . getSumCost $ events)

getDetailEvents :: [String] -> String
getDetailEvents [] = ""
getDetailEvents (event:events) = "\n  Expenses:Food:School +" ++ getCost event ++ " CNY" ++ getDetailEvents events


-- 很 rough 的一个判断函数
guessTime :: String -> String
guessTime event
  | 5 <= hour && hour <= 10 = "早餐"
  | 11 <= hour && hour <= 14 = "中餐"
  | 15 <= hour && hour <= 20 = "晚餐"
  | otherwise = "夜宵"
  where hour = read (getTimeList event !! 0) :: Int

splitEvent :: String -> Char -> [String]
splitEvent event deli = wordsWhen (== deli) event

getDate :: String -> String
getDate line = splitEvent line ' ' !! 0

getTime :: String -> String
getTime line = splitEvent line ' ' !! 1

-- 分成 h m s
getTimeList :: String -> [String]
getTimeList line = splitEvent (getTime line) ':'

getEvent :: String -> String
getEvent line = splitEvent line ' ' !! 2

getCost :: String -> String
getCost line = tail (splitEvent line ' ' !! 4)

getSumCost :: [String] -> Float
getSumCost [] = 0
getSumCost (x:xs) = (read . getCost $ x :: Float) + getSumCost xs


-- limitTime 以小时算
-- 根据商户名称和交易时间分类
groupByEventTime :: [String] -> Int -> [[String]]
groupByEventTime lineList limitTime = groupBy (\fst' snd' -> let fstEvent = getEvent fst'
                                                                 sndEvent = getEvent snd'
                                                                 fstMain = takeWhile (/='/') fstEvent
                                                                 sndMain = takeWhile (/='/') sndEvent

                                                                 equalString = [ x | x<-fstEvent, y<-sndEvent, x==y]
                                                                 l1 = length equalString
                                                                 l2 = max (length fstEvent) (length sndEvent)

                                                                 fstTimeList = getTimeList fst'
                                                                 sndTimeList = getTimeList snd'
                                                                 fstS = convertT (fstTimeList !! 0) (fstTimeList !! 1) (fstTimeList !! 2)
                                                                 sndS = convertT (sndTimeList !! 0) (sndTimeList !! 1) (sndTimeList !! 2)
                                                             in (fstMain == sndMain || 2*l1 > l2 ) && abs (fstS - sndS) < limitTime * 60 * 60) lineList


-- 将时间转化为秒
convertT :: String -> String -> String -> Int
convertT h m s = (read h :: Int) * 60 * 60 + (read m :: Int) * 60 + (read s :: Int)
