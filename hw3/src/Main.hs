{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad.Cont       (liftIO)
import           Data.Set                 (Set, fromList)
import           HW3.Action               (HIO (..), HiPermission (..))
import           HW3.Evaluator            (eval)
import           HW3.Parser               (parse)
import           HW3.Pretty               (prettyValue)
import           System.Console.Haskeline (InputT, defaultSettings,
                                           getExternalPrint, getInputLine,
                                           runInputT)

permissions :: Set HiPermission
permissions = fromList [AllowRead, AllowWrite]

main :: IO ()
main = runInputT defaultSettings test

loop :: InputT IO ()
loop = getInputLine "hi> " >>= \case
  Nothing     -> return ()
  Just "quit" -> return ()
  Just "exit" -> return ()
  Just input  -> handle input >> loop

handle :: String -> InputT IO ()
handle input = do
  printer <- getExternalPrint
  liftIO $ case parse input of
    Left er    -> printer $ show er
    Right expr -> runHIO (eval expr) permissions >>= liftIO <$> \case
      Left er      -> printer $ show er
      Right result -> printer $ show $ prettyValue result

test :: InputT IO ()
test = do
  handle "mul(2, 10)"
  handle "sub(1000, 7)"
  handle "div(3, 5)"
  handle "div(add(10, 15.1), 3)"
  handle "add(500, 12)"
  handle "sub(10, 100)"
  handle "mul(23, 768)"
  handle "div(57, 190)"
  handle "div(add(mul(2, 5), 1), sub(11,6))"
  handle "sub(1)"
  handle "sub(1, 2, 3)"
  handle "div(1, 0)"
  handle "div(1, sub(5, 5))"
  handle "15(2)"
  handle "sub(10, add)"
  handle "100"
  handle "-15"
  handle "add(100, -15)"
  handle "add(3, div(14, 100))"
  handle "div(10, 3)"
  handle "sub(mul(201, 11), 0.33)"
  handle "not(true)"
  handle "and(true, false)"
  handle "or(true, false)"
  handle "equals(10, 10)"
  handle "equals(false, false)"
  handle "equals(3, 10)"
  handle "equals(1, true)"
  handle "less-than(3, 10)"
  handle "less-than(false, true)"
  handle "less-than(false, 0)"
  handle "false"
  handle "equals(add(2, 2), 4)"
  handle "less-than(mul(999, 99), 10000)"
  handle "if(greater-than(div(2, 5), div(3, 7)), 1, -1)"
  handle "and(less-than(0, 1), less-than(1, 0))"
  handle "if(true, add, mul)"
  handle "if(true, add, mul)(10, 10)"
  handle "if(false, add, mul)(10, 10)"
  handle "equals(add, add)"
  handle "equals(add, mul)"
  handle "2 + 2"
  handle "2 + 2 * 3"
  handle "(2 + 2) * 3"
  handle "2 + 2 * 3 == (2 + 2) * 3"
  handle "10 == 2*5 && 143 == 11*13"
  handle "add(10, 4) + 2 * 3"
  handle "2 + add(10, 4) * 3"
  handle "2 + 3 * add(10, 4)"
  handle "add(10 - 4 * 2, 7)"
  handle " true&&( true&&false ) "
  handle "length(\"Hello World\")"
  handle "to-upper(\"Hello World\")"
  handle "to-lower(\"Hello World\")"
  handle "reverse(\"stressed\")"
  handle "trim(\" Hello World \")"
  handle "\"Hello\" + \"World\""
  handle "\"Cat\" * 5"
  handle "\"/home/user\" / \"hi\""
  handle "\"\\\"\""
  handle "\"Hello World\"(0)"
  handle "\"Hello World\"(7)"
  handle "\"Hello World\"(0, 5)"
  handle "\"Hello World\"(2, 4)"
  handle "list(1, 2, 3)"
  handle "range(5, 10.3)"
  handle "fold(add, [11, 22, 33])"
  handle "fold(mul, [11, 22, 33])"
  handle "fold(div, [11, 22, 33])"
  handle "length([1, true, \"Hello\"])"
  handle "reverse([1, true, \"Hello\"])"
  handle "[1, 2] + [3, 4, 5]"
  handle "[0, \"x\"] * 3"
  handle "list(1, 2, 3, 4, 5)"
  handle "fold(add, [2, 5] * 3)"
  handle "fold(mul, range(1, 10))"
  handle "[0, true, false, \"hello\", \"world\"](2, 4)"
  handle "reverse(range(0.5, 70/8))"
  handle "{ \"width\": 120, \"height\": 80 }"
  handle "{ 1: true, 3: true, 4: false }"
  handle "{ \"width\": 120, \"height\": 80 }.width"
  handle "keys({ \"width\": 120, \"height\": 80 })"
  handle "values({ \"width\": 120, \"height\": 80 })"
  handle "count(\"XXXOX\")"
  handle "count([true, true, false, true])"
  handle "invert({ \"x\": 1, \"y\" : 2, \"z\": 1 })"
  handle "count(\"Hello World\").o"
  handle "invert(count(\"big blue bag\"))"
  handle "fold(add, values(count(\"Hello, World!\")))"
  handle "pack-bytes([ 3, 255, 158, 32 ])"
  handle "unpack-bytes([# 10 20 30 #])"
  handle "encode-utf8(\"Hello!\")"
  handle "decode-utf8([# 48 65 6c 6c 6f #])"
  handle "decode-utf8([# c3 28 #])"
  handle "[# 00 ff #] + [# 01 e3 #]"
  handle "[# 00 ff #] * 3"
  handle "[# 00 ff 23 43 63 57 #](3, 5)"
  handle "[# 00 ff 23 43 63 57 #](3)"
  handle "zip(encode-utf8(\"Hello, World!\" * 1000))"
  handle "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])"
  handle "deserialise(serialise([1, 2] + [3, 4, 5]))"
  handle "mkdir(\"tmp\")!"
  handle "read(\"tmp\")!"
  handle "mkdir(\"tmp/a\")!"
  handle "mkdir(\"tmp/b\")!"
  handle "read(\"tmp\")!"
  handle "encode-utf8(\"Hello\")"
  handle "write(\"tmp/hi.txt\", \"Hello\")"
  handle "write(\"tmp/hi.txt\", \"Hello\")!"
  handle "cd(\"tmp\")!"
  handle "read(\"hi.txt\")!"
  handle "read"
  handle "read(\"hi.txt\")"
  handle "read(\"hi.txt\")!"
  handle ""