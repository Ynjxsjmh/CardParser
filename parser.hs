-- ghc parser.hs && .\parser.exe input.txt
-- input.txt 里不能有 tab
-- 商户名称里面可能有“套餐1”这种形式，但是此脚本不支持中间的空格，需要去掉。
-- 否则会报“Prelude.read: no parse”这个错误

import System.Environment (getArgs)
import System.IO
import Data.List
import Text.Printf

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

  outh <- openFile ((getDir . head' $ args) ++ "result.bean") WriteMode
  hSetEncoding outh utf8

  let modified = convert . groupByDay . mfilter . lines $ text
    in hPutStrLn outh (unlines . unlines' $ modified)

  hClose file
  hClose outh


unlines' :: [[String]] -> [String]
unlines' [] = []
unlines' (x:xs) = x ++ unlines' xs

-- 去除不需要的行
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
filterTransfer lineList = filter (\line -> not (isInfixOf "补助流水" line) ) lineList

-- 去除空行
filterNull :: [String] -> [String]
filterNull [] = []
filterNull lineList = filter (\line -> length line > 0) lineList

-- 去除那些第一个不是 date 形式的行
-- 比如 “历史流水查询：开始时间”
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
-- 将这些 events 转换成 beancount 格式
combine :: [String] -> String
combine [] = ""
combine events = printf "%s * %s\n  datetime: %s %s%s" date (getEventsName events) date time (getDetail events)
  where timeList = splitEvent (getDate . head' $ events) '/'
        complete numberString
          | length numberString < 2 = "0" ++ numberString
          | otherwise = numberString
        date = timeList !! 0 ++ "-" ++ complete (timeList !! 1) ++ "-" ++ complete (timeList !! 2)
        time = (getTime . head' $ events)

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
getEventsName lineList
  | isInfixOf "浴池" (head' lineList) = flatten . remove_dups $ (map getEvent lineList)
  | otherwise = flatten $ (map getEvent lineList)

-- events 是同一天内相同的事件
getDetail :: [String] -> String
getDetail [] = ""
getDetail events
  | isInfixOf "浴池" event = "\n  Expenses:Health:Bath +" ++ cost ++ " CNY\n" ++
                             "  Assets:CampusCard:JLU -" ++ cost ++ " CNY\n\n"
  | isInfixOf "网络使用费" event = "\n Expenses:Service:Internet +" ++ (getTransfer event) ++ " CNY\n" ++
                                 "  Assets:CampusCard:JLU -" ++ (getTransfer event) ++ " CNY\n\n"
  | isInfixOf "银行转账" event = "\n  Assets:Bank:CN:BOC -" ++ (getTransfer event) ++ " CNY\n" ++
                                 "  Assets:CampusCard:JLU +" ++ (getTransfer event) ++ " CNY\n\n"
  | otherwise  = getDetailEvents events ++
                 "\n  Assets:CampusCard:JLU -" ++ cost ++ " CNY\n\n"
  where event = head' events
        cost = printf "%.2f" (getSumCost events) :: String
        getTransfer event = splitEvent event ' ' !! 3

getDetailEvents :: [String] -> String
getDetailEvents [] = ""
getDetailEvents (event:events) = getExpensesType event ++ (getCost event) ++ " CNY" ++ getDetailEvents events
  where getExpensesType event
          |  5 <= hour && hour <= 10 = "\n  Expenses:Food:Breakfast +"
          | 11 <= hour && hour <= 14 = "\n  Expenses:Food:Dinner +"
          | 15 <= hour && hour <= 20 = "\n  Expenses:Food:Lunch +"
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
-- 商户名称判等规则：
--   1. 两笔交易商户名称第一个 / 前面是一样的
--   2. 两笔交易商户名称位对位字符比较相同达到一定标准
-- 交易时间判等规则：一小时之内
groupByEventTime :: [String] -> Int -> [[String]]
groupByEventTime lineList limitTime = groupBy (\fst' snd' ->
                                                 let fstEvent = getEvent fst'
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
