module Main where

import           Data.Either     (fromRight)
import           HW3.Evaluator   (eval)
import           HW3.Parser      (parse)
import           HW3.Pretty      (prettyValue)

handle :: String -> IO ()
handle text = do
  putStrLn $ "=== " <> text <> " ==="
  let syntaxTree = either (const Nothing) Just (parse text)
--  maybe (putStrLn "Can't parse expression.") print syntaxTree
  value <- mapM eval syntaxTree
  maybe (putStrLn "Can't calculate expression.") print value
  let pretty = fmap prettyValue $ fromRight Nothing $ sequence value
  maybe (putStrLn "Can't evaluate expression.") print pretty
  putStrLn ""

main :: IO ()
main = do
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
    handle ""
--    handle ""
--    handle ""
--    handle ""
--    handle ""
--    handle ""
--    handle ""
