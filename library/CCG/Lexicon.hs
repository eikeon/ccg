module CCG.Lexicon where

import qualified Data.Map as M

import CCG.Category
import Morte.Core (Expr, Path)

type Lexicon = M.Map String [(Category, Expr Path)]
