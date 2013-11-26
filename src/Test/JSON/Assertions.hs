{-# LANGUAGE GADTs #-}
module Test.JSON.Assertions
    ( -- * Tests and Traversals
      key
    , nth
    , assertEq
    , stop
    , jsonTest

      -- * Test Interpreters
    , testJSON

    , JSONTest
    ) where

import Control.Monad.Indexed (IxFunctor(..), (>>>=))
import Control.MonadPlus.Indexed.Free (IxFree(..))
import Data.Monoid (First)

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Text as Text

--------------------------------------------------------------------------------
data JSONF i j a where
  Key :: String -> (i -> j) -> (j -> a) -> JSONF i j a
  Index :: Int -> (i -> j) -> (j -> a) -> JSONF i j a
  Assert :: (Aeson.Value -> Either String ()) -> a -> JSONF i i a
  Stop :: JSONF i () a

instance IxFunctor JSONF where
  imap g (Key keyS f k) = Key keyS f (g . k)
  imap g (Index n f k) = Index n f (g . k)
  imap f (Assert p k) = Assert p (f k)
  imap _ Stop = Stop


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
-- | Finalize a 'JSONTest' by calling 'stop' at the end.
jsonTest :: JSONTest i j a -> JSONTest i () a
jsonTest = (>>>= const stop)

--------------------------------------------------------------------------------
testJSON :: Aeson.ToJSON i => JSONTest i j a -> i -> [String]
testJSON tests env = go tests (Aeson.toJSON env) env "subject"

 where

  go :: JSONTest i j a -> Aeson.Value -> i -> String -> [String]

  go (Pure _) _ _ _ = []

  go (Free (Key keyS f k)) actual expected descr =
    tryLens (Aeson.key (Text.pack keyS)) f actual expected k $
      descr ++ "[\"" ++ keyS ++ "\"]"

  go (Free (Index n f k)) actual expected descr =
    tryLens (Aeson.nth n) f actual expected k $
      descr ++ " failed to match any targets"

  go (Free (Assert p k)) actual expected descr =
    either
      (return . ((descr ++ " failed assertion\n") ++))
      (const $ go k actual expected descr)
      (p actual)

  go (Free Stop) _ _ _ = []

  go (Plus steps) actual expected descr =
    concatMap (\s -> go s actual expected descr) steps

  tryLens :: Lens.Getting (First Aeson.Value) Aeson.Value Aeson.Value
          -> (i -> j) -> Aeson.Value
          -> i -> (j -> JSONTest j k a)
          -> String
          -> [String]

  tryLens l f actual expected k path =
    case Lens.preview l actual of
      Nothing -> [path ++ " failed to match any targets"]
      Just matched ->
        go (k (f expected)) matched (f expected) path
