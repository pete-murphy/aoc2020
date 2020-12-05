module AOCPrelude
  ( module P,
  )
where

import Control.Applicative as P
import Control.Monad as P
import Control.Monad.Combinators as P hiding (endBy, many, option, some)
import Control.Monad.Except as P
import Control.Monad.State as P
import Data.Bifunctor as P
import Data.Char as P
import Data.Coerce as P
import Data.Containers.ListUtils as P
import Data.Either as P
import Data.Foldable as P
import Data.Function as P
import Data.Functor as P
import Data.IntMap as P (IntMap)
import Data.IntSet as P (IntSet)
import Data.Ix as P hiding (index)
import Data.Kind as P
import Data.List as P
import Data.List.NonEmpty as P (NonEmpty (..), nonEmpty)
import Data.List.Split as P hiding (oneOf, sepBy)
import Data.Map as P (Map)
import Data.Maybe as P
import Data.Ord as P
import Data.Semigroup as P
import Data.Set as P (Set)
import Data.Text as P (Text)
import Data.Text.Encoding as P (decodeUtf8, encodeUtf8)
import Data.Traversable as P
import Data.Tuple as P
import Data.Void as P
import Debug.Trace as P
import GHC.Base as P hiding (Any, foldr, mapM, sequence)
import GHC.Enum as P
-- import GHC.Char as P
import GHC.Generics as P (Generic)
import GHC.Num as P
import GHC.Real as P
import GHC.Show as P
import Numeric.Natural as P
import System.IO as P
import System.IO.Unsafe as P
import Text.Megaparsec as P (Parsec, anySingle, anySingleBut, eof, noneOf, oneOf, satisfy, try)
import Text.Megaparsec.Char as P (asciiChar, binDigitChar, char, digitChar, hexDigitChar, newline, octDigitChar, space, space1, string, string')
import Text.Megaparsec.Char.Lexer as P (binary, decimal, float, hexadecimal, octal, scientific, signed)
import Text.Printf as P
import Text.Read as P (read, readMaybe)
import Unsafe.Coerce as P
