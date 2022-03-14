{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Foldl.Exceptions

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Hedgehog
import System.Exit
import System.IO

import qualified Control.Foldl as L

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- checkParallel $$discover
    when (not ok) exitFailure

prop_1 :: Property
prop_1 = withTests 1 $ property $ do
    let f x = if x < 10 then return x else throw Overflow
    let xs = [1, 2, 500, 4] :: [Integer]

    -- Since (f 500) produces an exception, the following fold fails:
    let fold1 = L.premapM f (L.generalize L.list)
    liftIO (try (L.foldM fold1 xs)) >>= (=== Left Overflow)

    -- By applying 'untilFirstException', we can produce a new fold that returns
    -- the intermediate result at the point where the exception occurs.
    let fold2 = exHalt_ fold1
    liftIO (L.foldM fold2 xs) >>= (=== [1, 2])

prop_2 :: Property
prop_2 = withTests 1 $ property $ do
    let f x = if x < 10 then return x else throw Overflow
    let xs = [1, 2, 500, 4] :: [Integer]

    let fold1 = L.premapM f (L.generalize L.list)
    let fold2 = exHalt @ArithException fold1

    liftIO (L.foldM fold2 xs) >>= (=== (Just Overflow, [1, 2]))

prop_3 :: Property
prop_3 = withTests 1 $ property $ do
    let f x = if x < 10 then return x else throw Overflow
    let xs = [1, 2, 500, 4] :: [Integer]

    -- Since (f 500) produces an exception, the following fold fails:
    let fold1 = L.premapM f (L.generalize L.list)
    liftIO (try (L.foldM fold1 xs)) >>= (=== Left Overflow)

    -- By applying 'exSkip_', we can produce a new fold that produces
    -- a result from all steps that /don't/ fail:

    let fold2 = exSkip_ fold1
    liftIO (L.foldM fold2 xs) >>= (=== [1, 2, 4])

prop_4 :: Property
prop_4 = withTests 1 $ property $ do
    let f x = if x < 10 then return x else throw Overflow
    let xs = [1, 2, 500, 4] :: [Integer]

    -- Since (f 500) produces an exception, the following fold fails:
    let fold1 = L.premapM f (L.generalize L.list)
    liftIO (try (L.foldM fold1 xs)) >>= (=== Left Overflow)

    -- By applying 'exSkip', we can produce a new fold that produces
    -- a result from all steps that /don't/ fail:

    let fold2 = exSkip @ArithException fold1
    liftIO (L.foldM fold2 xs) >>= (=== ([Overflow], [1, 2, 4]))
