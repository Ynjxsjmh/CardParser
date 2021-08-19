-- ghc parser.hs && .\parser.exe input.txt
-- input.txt 里不能有 tab
-- 商户名称里面可能有“套餐 1”，“基础园超市 31”,“负一层/负1A04”这种形式，但是此脚本不支持中间的空格，需要去掉。
-- 否则会报“Prelude.read: no parse”这个错误，可以尝试通过二分查找处理输入文件定位哪里不符合

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
filterTransfer lineList = filter (\line -> not (isInfixOf "补助流水" line
                                                || isInfixOf "解挂" line
                                                || isInfixOf "挂失" line
                                                || isInfixOf "补记流水" line) ) lineList

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
combine events = printf "%s * %s\n  datetime: \"%s %s\"%s" date (getEventsName events) date time (getDetail events)
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

getEventsName :: [String] -> String
getEventsName lineList
  | (isInfixOf "浴池" (head' lineList))
    || (isInfixOf "银行转账" (head' lineList)) = formattedEventsName
  | otherwise = "\"" ++ (guessEventType . head' $ lineList) ++ "\" " ++ formattedEventsName
  where eventsName = (map (takeWhile (/='/')) (map getEvent lineList))
        formattedEventsName = "\"" ++ intercalate " ￭ " (remove_dups eventsName) ++ "\""

-- events 是同一天内相同的事件
getDetail :: [String] -> String
getDetail [] = ""
getDetail events
  | isInfixOf "浴池" event = "\n  Expenses:Health:Bath +" ++ costSum ++ " CNY\n" ++
                             "  Assets:CampusCard:JLU -" ++ costSum ++ " CNY\n\n"
  | isInfixOf "网络使用费" event = "\n  Expenses:Service:Internet +" ++ costSum ++ " CNY\n" ++
                                 "  Assets:CampusCard:JLU -" ++ costSum ++ " CNY\n\n"
  | isInfixOf "银行转账" event = "\n  Assets:Bank:CN:BOC -" ++ transferCostSum ++ " CNY\n" ++
                                 "  Assets:CampusCard:JLU +" ++ transferCostSum ++ " CNY\n\n"
  | otherwise  = getDetailEvents events ++
                 "\n  Assets:CampusCard:JLU -" ++ costSum ++ " CNY\n\n"
  where event = head' events
        getSumCost :: [String] -> Float
        getSumCost [] = 0
        getSumCost (x:xs) = (read x :: Float) + getSumCost xs
        costSum = printf "%.2f" (getSumCost (map getCost events)) :: String
        getTransfer event = splitEvent event ' ' !! 3
        transferCostSum = printf "%.2f" (getSumCost (map getTransfer events)) :: String

getDetailEvents :: [String] -> String
getDetailEvents [] = ""
getDetailEvents (event:events) = getExpensesDetail event ++ cost ++ " CNY  ;; " ++ getEvent event ++ getDetailEvents events
  where cost = printf "%.2f" (read . getCost $ event :: Float) :: String
        getExpensesDetail event
          | eventType == "Breakfast" = "\n  Expenses:Food:Breakfast +"
          | eventType == "Lunch"     = "\n  Expenses:Food:Lunch +"
          | eventType == "Dinner"    = "\n  Expenses:Food:Dinner +"
          | otherwise                = "\n  Expenses:Misc +"
          where eventType = guessEventType event

guessEventType :: String -> String
guessEventType event
  | isFood &&  5 <= hour && hour <= 10 = "Breakfast"
  | isFood && 11 <= hour && hour <= 14 = "Lunch"
  | isFood && 15 <= hour && hour <= 20 = "Dinner"
  | otherwise = "Misc"
  where hour = read (getTimeList event !! 0) :: Int
        isFood = (isInfixOf "食堂" event) || (isInfixOf "餐厅" event)
                 || (isInfixOf "新楼" event && isInfixOf "二楼" event)

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

-- 去除掉前面的负号
-- 因为正常的吃饭交易都是减钱
getCost :: String -> String
getCost line = tail (splitEvent line ' ' !! 4)

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
