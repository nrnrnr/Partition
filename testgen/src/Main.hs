{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs
import System.Exit
import Data.Int
import Data.List
import Data.Char
import Test.QuickCheck
import Control.Monad

data SchemeValue = SAtom SchemeAtom
                 | SList [SchemeValue]
                 deriving Show
data SchemeAtom = SInt Int32
                | SSymbol Symbol
                | SNil
                deriving Show
newtype Symbol = Symbol String
               deriving Show

instance Arbitrary SchemeValue where
  arbitrary = sized value
    where value 0 = liftM SAtom arbitrary
          value n | n>0 = frequency [ (2, liftM SAtom arbitrary)
                                    , (1, liftM SList $ vectorOf (n `div` 4) (value (n - 1)))
                                    ]
instance Arbitrary SchemeAtom where
  arbitrary = oneof [ liftM SInt arbitrary
                    , liftM SSymbol arbitrary
                    , pure SNil
                    ]

instance Arbitrary Symbol where
  arbitrary = sized (\n -> elements (take (n + 1) symbols))
    where symbols = map Symbol $ ["a", "b", "c", "d", "e"] ++ map (("s" ++) . show) [6 ..]

renderUnquoted :: SchemeValue -> String
renderUnquoted (SAtom a) = renderAtom a
  where renderAtom SNil = "()"
        renderAtom (SInt i) = Prelude.show i
        renderAtom (SSymbol (Symbol s)) = s
renderUnquoted (SList vs) = "(" ++ concat (intersperse " " $ map renderUnquoted vs) ++ ")"

renderUnquoteds :: [SchemeValue] -> String
renderUnquoteds vs = "(" ++ inner vs ++ ")"
  where inner = concat . intersperse " " . map renderUnquoted


renderUnzip :: [(SchemeValue, SchemeValue)] -> String
renderUnzip xs = "'(" ++ concat (intersperse " " (map renderPair xs)) ++ ")"
  where renderPair (v, w) = "(" ++ renderUnquoted v ++ " " ++ renderUnquoted w ++ ")"

renderFlatten :: [SchemeValue] -> String
renderFlatten vs = "'" ++ renderUnquoteds vs

renderZip :: ([SchemeValue], [SchemeValue]) -> String
renderZip (xs, ys) = "'" ++ renderUnquoteds xs ++ " '" ++ renderUnquoteds ys

renderDrop :: (Int, [SchemeValue]) -> String
renderDrop (n, xs) = Prelude.show n ++ " " ++ "'" ++ renderUnquoteds xs

renderCountall :: (SchemeValue, [SchemeValue]) -> String
renderCountall (x, xs) = "'" ++ renderUnquoted x ++ " '" ++ renderUnquoteds xs

renderMaximallyDistantPoint :: [(SchemeValue, SchemeValue)] -> String
renderMaximallyDistantPoint = renderUnzip


twoSameLength :: Gen ([SchemeValue], [SchemeValue])
twoSameLength = do
  n <- frequency [ (1, arbitrarySizedNatural)
                 , (3, fmap (+1) arbitrarySizedNatural)
                 ]
  let g = (vectorOf n arbitrary)
  liftArbitrary2 g g

listLikelyContaining :: Gen (SchemeValue, [SchemeValue])
listLikelyContaining = do
  x <- liftM SAtom $ arbitrary
  xs <- listOf $ frequency [ (6, arbitrary)
                           , (1, pure x)
                           ]
  pure (x, xs)

moreDistant :: (Int32, Int32) -> (Int32, Int32)
moreDistant (x, y) =
  if x - y < 0
  then (x, y + 1)
  else (x + 1, y)

listWithUniqueMaximalPoint :: Gen [(SchemeValue, SchemeValue)]
listWithUniqueMaximalPoint = do
  ps@((x0, y0) : points) <- listOf1 $ arbitrary
  let (x, y) = maximalPoint (x0, y0) points
  let inj = SAtom . SInt
  pure $ map (\(x, y) -> (inj x, inj y)) $ moreDistant (x, y) : ps

maximalPoint :: (Int32, Int32) -> [(Int32, Int32)] -> (Int32, Int32)
maximalPoint (x, y) [] = (x, y)
maximalPoint (x, y) ((x0, y0) : points) =
  if abs (x - y) >= abs (x0 - y0)
  then maximalPoint (x, y) points
  else maximalPoint (x0, y0) points

data Options = Options { size :: Int
                       , suiteSize :: Int
                       , testName :: String
                       }
               deriving (Data, Typeable)
options = Options { size = 10 &= explicit &= name "size" &= help "Size parameter to generator"
                  , suiteSize = 5 &= help "Number of tests to generate"
                  , testName = "unzip" &= help "Which test to generate"
                  }
          &= summary "Test Generator"
          &= program "testgen"

doTest :: Int -> Int -> (Gen a, a -> String) -> IO ()
doTest s n (gen, renderer) = do
  suite <- generate $ resize s $ vectorOf n gen
  mapM_ putStrLn $ map renderer $ suite

main :: IO ()
main = do
  Options { size = s, suiteSize = n, testName = tName } <- cmdArgs options
  let t = doTest s n
  case map toLower tName
    of "unzip" -> t (arbitrary, renderUnzip)
       "flatten" -> t (arbitrary, renderFlatten)
       "zip" -> t (twoSameLength, renderZip)
       "drop" -> t (liftArbitrary2 arbitrarySizedNatural arbitrary, renderDrop)
       "countall" -> t (listLikelyContaining, renderCountall)
       "maximally-distant-point" -> t (listWithUniqueMaximalPoint, renderMaximallyDistantPoint)
       name -> die $ "Unsupported test '" ++ name ++ "'"
