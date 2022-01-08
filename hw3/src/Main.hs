module Main where

import           Data.Either     (fromRight)
import           HW3.Base        (HiValue (..))
import           HW3.Evaluator   (eval)
import           HW3.Parser      (parse)
import           HW3.Pretty      (prettyValue)

handle :: String -> IO ()
handle text = do
  putStrLn $ "=== " <> text <> " ==="
  let syntaxTree = either (const Nothing) Just (parse text)
  maybe (putStrLn "Can't parse expression.") print syntaxTree
  value <- mapM eval syntaxTree
  maybe (putStrLn "Can't calculate expression.") print value
  let pretty = fmap prettyValue $ fromRight Nothing $ sequence value
  maybe (putStrLn "Can't prettify expression.") print pretty
  putStrLn ""

main :: IO ()
main = do
    handle "div(add(10, 15.1), 3)"
    handle "div(add(mul(2, 5), 1), sub(11,6))"
    print $ prettyValue $ HiValueNumber $ 0.0
    print $ prettyValue $ HiValueNumber $ 1.0
    print $ prettyValue $ HiValueNumber $ -8.15
    print $ prettyValue $ HiValueNumber $ -1/7
    print $ prettyValue $ HiValueNumber $ -1/5
    print $ prettyValue $ HiValueNumber $ 24 + 3/11
    print $ prettyValue $ HiValueNumber $ -24 + 3/11
