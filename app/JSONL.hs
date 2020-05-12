module JSONL (decode, decodeFile) where

import BasicPrelude hiding (first)
import Control.Monad.Combinators (many)
import Data.Aeson (JSONPath, FromJSON, Value)
import Data.Aeson.Internal (IResult(..), ifromJSON, formatError)
import Data.Aeson.Parser (json)
import Data.Aeson.Types (parseEither)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.ByteString.Char8 (Parser, endOfInput, parseOnly)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Prelude as P


decodeFile :: FromJSON a => FilePath -> IO (Either String [a])
decodeFile = map decode . BS.readFile

-- Based on https://hackage.haskell.org/package/aeson-1.4.7.1/docs/src/Data.Aeson.html#eitherDecodeStrict
-- incl. inlined eitherDecodeWith, eitherFormatError
decode :: FromJSON a => ByteString -> Either String [a]
decode bs = do
    vals <- parseOnly jsonlEOF bs

    mapLeft P.unlines
      . combineErrors
      $ zipWith convertElement [1..] vals

  where
    mapLeft = first

    -- convertElement :: Int -> Value -> Either String a
    convertElement lineNo val
      = case ifromJSON val of
             ISuccess x      -> pure x
             IError path msg -> Left ("Line " ++ show lineNo ++ ": " ++ formatError path msg)

    -- Helper function for collecting a list of errors or list of results
    combineErrors :: [Either l r] -> Either [l] [r]
    combineErrors = foldr f (Right []) where
      f :: Either l r -> Either [l] [r] -> Either [l] [r]
      f (Left l0) (Left ls) = Left (l0:ls)
      f (Left l0) (Right _) = Left [l0]
      f (Right r0) (Left ls) = Left ls
      f (Right r0) (Right rs) = Right (r0:rs)


-- Based on https://hackage.haskell.org/package/aeson-1.4.7.1/docs/src/Data.Aeson.Parser.Internal.html#jsonEOF
jsonlEOF :: Parser [Value]
jsonlEOF = many (json <* skipSpace) <* skipSpace <* endOfInput

-- Copy of https://hackage.haskell.org/package/aeson-1.4.7.1/docs/src/Data.Aeson.Parser.Internal.html#skipSpace
skipSpace :: Parser ()
skipSpace = skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

-- Copy of https://hackage.haskell.org/package/aeson-1.4.7.1/docs/src/Data.Aeson.Parser.Internal.html#eitherDecodeStrictWith with different types
eitherDecodeWith
  :: Parser b
    -> (b -> IResult a)
    -> ByteString
    -> Either (JSONPath, String) a
eitherDecodeWith p to s =
    case either (IError []) to (parseOnly p s) of
      ISuccess a      -> Right a
      IError path msg -> Left (path, msg)

-- Copy of https://hackage.haskell.org/package/aeson-1.4.7.1
eitherFormatError :: Either (JSONPath, String) a -> Either String a
eitherFormatError = either (Left . uncurry formatError) Right
