module AesonValueGen
    () where


import           Codd.Hashing.Types             ( Json(..) )
import           Data.Aeson                     ( Array
                                                , Object
                                                , Value(..)
                                                )
import qualified Data.HashMap.Strict           as KM
import           Data.Scientific                ( Scientific
                                                , base10Exponent
                                                , coefficient
                                                , scientific
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Vector                   as V
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

instance Arbitrary Value where
    arbitrary = sized arbValue

-- | Taken from https://github.com/ChickenProp/aeson/commit/a43af20b1554ecd2ac5b924d4893b2161e29a58b
arbValue :: Int -> Gen Value
arbValue n
    | n <= 1 = oneof
        [ pure Null
        , Bool <$> arbitrary
        , String <$> arbText
        , Number <$> arbScientific
        , pure emptyObject
        , pure emptyArray
        ]
    | otherwise = oneof [Object <$> arbObject n, Array <$> arbArray n]
arbText :: Gen Text
arbText = pack <$> arbitrary
arbScientific :: Gen Scientific
arbScientific = scientific <$> arbitrary <*> arbitrary
shrScientific :: Scientific -> [Scientific]
shrScientific s =
    map (uncurry scientific) $ shrink (coefficient s, base10Exponent s)
arbObject :: Int -> Gen Object
arbObject n = do
    p <- arbPartition (n - 1)
    KM.fromList
        <$> traverse (\m -> (,) <$> (pack <$> arbitrary) <*> arbValue m) p
arbArray :: Int -> Gen Array
arbArray n = do
    p <- arbPartition (n - 1)
    V.fromList <$> traverse arbValue p
arbPartition :: Int -> Gen [Int]
arbPartition k = case compare k 1 of
    LT -> pure []
    EQ -> pure [1]
    GT -> do
        first <- chooseInt (1, k)
        rest  <- arbPartition $ k - first
        shuffle (first : rest)


-- | The empty object.
emptyObject :: Value
emptyObject = Object KM.empty

-- | The empty array.
emptyArray :: Value
emptyArray = Array V.empty
