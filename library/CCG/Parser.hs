{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_GHC -Wall #-}

module CCG.Parser where

import           Control.Exception                (Exception)

import           Control.Monad.Trans.Except       (runExceptT, throwE)
import           Control.Monad.Trans.State.Strict (evalState)
import           Data.Maybe                       (catMaybes)
import           Data.Monoid
import           Data.Text.Buildable              (Buildable (..))
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy                   as Text
import           Data.Text.Lazy.Builder           (toLazyText)
import           Data.Typeable                    (Typeable)
import qualified Data.Map as M

import CCG.Category
import CCG.Lexicon

import Morte.Core (Const(..), Path(..), Expr(..))

data CellData = CellData {category :: Category, path :: ParsePath} deriving (Show, Eq)

instance Show ParsePath where
    show _ = ""
-- binary tree
data ParsePath = ParsePath CellData CellData Rule | ParseWord Text deriving (Eq)

-- Cell data down right diagonal
data Chart = Cell [CellData] Chart Chart Chart | End deriving (Show, Eq)

getColumn :: Chart -> [[CellData]]
getColumn   End                  = []
getColumn   (Cell val chart _ _) = val : getColumn chart

getRow :: Chart -> [[CellData]]
getRow      End                  = []
getRow      (Cell val _ chart _) = val : getRow chart

getDiagonal :: Chart -> [[CellData]]
getDiagonal End                  = []
getDiagonal (Cell val _ _ chart) = val : getDiagonal chart

-- get down in char by n cells
verticalStep :: (Num r, Eq r) => Chart -> r -> Chart
verticalStep chart 0 = chart
verticalStep End _   = End
verticalStep (Cell _ chart _ _) n = verticalStep chart $ n - 1

-- right down by n
diagonalStep :: (Num r, Eq r) => Chart -> r -> Chart
diagonalStep chart 0 = chart
diagonalStep End _   = End
diagonalStep (Cell _ _ chart _) n = diagonalStep chart $ n - 1

-- chartToList :: Chart -> [[[CellData]]]
-- chartToList End                  = []
-- chartToList chart'@(Cell _ chart _ _) = getRow chart' : chartToList chart


listToChartRow :: [(Text, [(Category, Expr Path)])] -> Chart
listToChartRow []     = End
listToChartRow ((word, cats):xs) = Cell (map (\(x', _) -> CellData x' (ParseWord word)) cats) End (listToChartRow xs) End

parse :: [Rule] -> Lexicon -> Text -> Chart
parse rules lexicon s = parseIter rules (listToChartRow lexemes)
    where lexemes = map (\w -> (w, lexicon M.! Text.unpack w)) $ Text.words s

parseIter :: [Rule] -> Chart -> Chart
-- parsing until pyramid have 1 block in top row (right link is End in first cell)
parseIter _ chart@(Cell _ _ End _) = chart
parseIter rules chart = parseIter rules (put_cell chart)
    where -- put cell until there is two cell under new
      put_cell (Cell _ _ End _) = End
      -- put cell with: one last at down, one new at right and one last on diagonal
      put_cell c@(Cell _ _ right _) = Cell (cellData c) c (put_cell right) right
      put_cell End = error "unexpected"
      cellData c@(Cell _ _ right _) = applyRulesToDerivations (getColumn c) (reverse $ getDiagonal right)
      cellData End = error "unexpected"
      applyRulesToDerivations downList diagList
            = catMaybes $ concat (zipWith applyRules downList diagList) -- unique
      applyRules lefts rights =
        [(\x -> CellData x (ParsePath ldata rdata rule)) <$> combinator rule left right |
            ldata@(CellData left  _) <- lefts,
            rdata@(CellData right _) <- rights,
            rule <- rules]

defaultCombinatorsSet :: [Rule]
defaultCombinatorsSet =
        [
            ForwardApplication, BackwardApplication,
            ForwardComposition, BackwardComposition,
            ForwardSubstitution, BackwardComposition,
            ForwardSubstitution, BackwardXSubstitution,
            HalfCoordination, Coordination,
            ForwardTypeRaisingComposition, BackwardTypeRaisingComposition,
            LeftForwardTypeRaisingComposition, LeftBackwardTypeRaisingComposition
        ]

-- | The specific parsing error
data ParseMessage
    -- | Parsing failed, returning the invalid token and the expected tokens
    = Parsing Text [Text]
    deriving (Show)

-- | Structured type for parsing errors
data ParseError = ParseError
    { parseMessage :: ParseMessage
    } deriving (Typeable)

instance Show ParseError where
    show = Text.unpack . toLazyText . build

instance Exception ParseError

instance Buildable ParseError where
 build (ParseError e) =
         "\n"
     <>  case e of
         Parsing t ts ->
                 "Parsing : " <> build (show t ) <> "\n"
             <>  "Expected: " <> build (show ts) <> "\n"
             <>  "\n"
             <>  "Error: Parsing failed\n"

sentenceFromText :: Text -> Either ParseError (Expr Path)
sentenceFromText text = evalState (runExceptT m) ()
    where
        m = do
            let parses = [text]
            case parses of
                _:_ -> return $ Const Star
                [] -> throwE (ParseError (Parsing "aaa" ["foo", "bar"]))
