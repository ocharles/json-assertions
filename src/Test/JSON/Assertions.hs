{-# LANGUAGE GADTs #-}
module Test.JSON.Assertions
    ( -- * Tests and Traversals
      key
    , nth
    , assertEq
    , stop

      -- * Test Interpreters
    , testJSON

    , JSONTest
    ) where

import Control.Monad.Indexed (IxFunctor(..))
import Control.MonadPlus.Indexed.Free (IxFree(..))

import qualified Control.Lens as Lens
import qualified Control.Lens.Aeson as Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

--------------------------------------------------------------------------------
data JSONF i j a where
  Key :: String -> (i -> j) -> (j -> a) -> JSONF i j a
  Index :: Int -> (i -> j) -> (j -> a) -> JSONF i j a
  Assert :: (Aeson.Value -> Either String ()) -> a -> JSONF i i a
  Stop :: JSONF i () a

instance IxFunctor JSONF where
  imap g (Key key f k) = Key key f (g . k)
  imap g (Index n f k) = Index n f (g . k)
  imap f (Assert p k) = Assert p (f k)
  imap f Stop = Stop


--------------------------------------------------------------------------------
type JSONTest = IxFree JSONF


--------------------------------------------------------------------------------
-- | Traverse into the value underneath a specific key in the JSON structure.
key :: String   -- ^ JSON Key
    -> (i -> j) -- ^ An associated morphism into a substructure of the test environment
    -> JSONTest i j j
key k f = Free (Key k f Pure)


--------------------------------------------------------------------------------
-- | Traverse the specific index of a JSON array
nth :: Int      -- ^ JSON array index
    -> (i -> j) -- ^ An associated morphism into a substructure of the test environment
    -> JSONTest i j j
nth i f = Free (Index i f Pure)


--------------------------------------------------------------------------------
-- | Assert that the current JSON value is exactly equal to the result of
-- calling 'Aeson.toJSON' on a value.
assertEq :: Aeson.ToJSON a => a -> JSONTest i i ()
assertEq expected =
  let expectedJSON = Aeson.toJSON expected
      p actual | actual == expectedJSON = Right ()
               | otherwise = Left $ unlines
                               [ "Expected: " ++ show expectedJSON
                               , "     Got: " ++ show actual
                               ]
  in Free (Assert p (Pure ()))


--------------------------------------------------------------------------------
-- | Using 'stop' discards the indices in the monad, which can help when you
-- need to 'isum' multiple tests that end in different states.
stop :: JSONTest a () r
stop = Free Stop


--------------------------------------------------------------------------------
testJSON :: Aeson.ToJSON i => JSONTest i j a -> i -> [String]
testJSON f env = go f (Aeson.toJSON env) env "subject"
 
 where

  go :: JSONTest i j a -> Aeson.Value -> i -> String -> [String]
 
  go (Pure _) _ _ _ = []
 
  go (Free (Key key f k)) actual expected descr =
    let descr' = descr ++ "[\"" ++ key ++ "\"]"
    in case Lens.preview (Aeson.key (Text.pack key)) actual of
         Nothing -> [descr' ++ " failed to match any targets"]
         Just matched ->
           go (k (f expected)) matched (f expected) descr'

  go (Free (Index n f k)) actual expected descr =
    let descr' = descr ++ "[" ++ show n ++ "]"
    in case Lens.preview (Aeson.nth n) actual of
         Nothing -> [descr' ++ " failed to match any targets"]
         Just matched ->
           go (k (f expected)) matched (f expected) descr'

  go (Free (Assert p k)) actual expected descr =
    either return (const []) (p actual)

  go (Free Stop) _ _ _ = []

  go (Plus steps) actual expected descr =
    concatMap (\s -> go s actual expected descr) steps
