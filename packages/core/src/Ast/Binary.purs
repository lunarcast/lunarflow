module Lunarflow.Ast.Binary where

import Prelude
import Data.Array as Array
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.List (List, foldr, snoc, (:))
import Data.List as List
import Data.Unfoldable (replicate)
import Lunarflow.Ast (AstF(..), DeBrujinLike)
import Lunarflow.Ast.Grouped (GroupedLike)
import Matryoshka (cata)

-- | Lambda calculus but represents using 0s and 1s.
type BinaryAst
  = { size :: Int
    , bits :: List Boolean
    }

-- | Show the bits inside a binary ast (for debugging)
showBinaryAst :: BinaryAst -> String
showBinaryAst ast = foldr go "" ast.bits
  where
  go bit = append if bit then "1" else "0"

-- | Convert an ast using de brujin indices to binary.
indexedToBinary :: forall v c l. DeBrujinLike v c l -> BinaryAst
indexedToBinary =
  cata case _ of
    Lambda _ body ->
      { size: body.size + 2
      , bits: false : false : body.bits
      }
    Call _ m n ->
      { size: 2 + m.size + n.size
      -- TODO: this is inefficient, stop doing it 
      , bits: false : true : m.bits <> n.bits
      }
    Var { index } ->
      { size: index + 2
      , bits: snoc (replicate (index + 1) true) false
      }

-- | Convert an ast where the lambdas are grouped into binary.
-- |
-- | Overview of encoding:
-- | (i*λ)A = 00((i - 1) * 1)0A
-- | A B    = 01AB
-- | i      = 1(i*1)0
-- | 
-- | Size comparassion for lambda heads:
-- | Args  Normal Grouped
-- |  1       2      3
-- |  2       4      4
-- |  3       6      5
-- |  4       8      6
-- |  n      2n    n + 2   
groupedToBinary :: forall v c a l. GroupedLike v c a l -> BinaryAst
groupedToBinary =
  cata case _ of
    Lambda { args } body ->
      { size: body'.size + argCount + 2
      , bits: false : false : replicate (argCount - 1) true <> false : body'.bits
      }
      where
      argCount = List.length args

      body' = case body.bits of
        false : b : bits -> { size: body.size - 1, bits: false : bits }
        _ -> body
    Call _ n m ->
      { size: 2 + n.size + m.size
      , bits: false : true : n.bits <> m.bits
      }
    Var { index } ->
      { size: index + 2
      , bits: snoc (replicate (index + 1) true) false
      }

-- | Actually store the binary data in a buffer.
toBuffer :: BinaryAst -> ArrayBuffer
toBuffer { size, bits } = toBufferImpl { size: size - 1, bits: Array.fromFoldable bits }

---------- Foreign imports
foreign import toBufferImpl :: { size :: Int, bits :: Array Boolean } -> ArrayBuffer
