{-# LANGUAGE OverloadedStrings #-}

module Day05.Main where
import           Data.Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import     qualified       Data.Attoparsec.Text as Parser
import           Data.Attoparsec.Text

main :: IO ()
main = do
  [input] <- Text.lines <$> TextIO.readFile "src/Day05/input.txt"
  let (Right stable) = removeReactionsUntilStable input
  let remainingUnits = Text.length stable
  putStrLn $ "Remaining units after fully reacting: " ++ (show remainingUnits)

removeReactionsUntilStable :: Text -> Either String Text
removeReactionsUntilStable prev = do
  next <- removeReactions prev
  if (prev == next) then pure prev else removeReactionsUntilStable next

removeReactions :: Text -> Either String Text
removeReactions = parseOnly parser
  where
    parser :: Parser Text
    parser = do
      remainingText <- many' (choice [reactable, Parser.take 1])
      pure $ Text.intercalate "" remainingText
    reactable :: Parser Text
    reactable = do
      c1 <- anyChar
      c2 <- anyChar
      if (c1 /= c2 && (toUpper c1) == (toUpper c2))
        then pure ""
        else fail "no reaction"


