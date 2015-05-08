module MudTests.Util.Misc where

import Mud.Util.Misc

import Data.IORef (newIORef, readIORef, writeIORef)


test_mWhen_IO_True :: IO Bool
test_mWhen_IO_True = do
    ref <- newIORef 'a'
    mWhen (return True) $ writeIORef ref 'A'
    val <- readIORef ref
    return $ val == 'A'


test_mWhen_IO_False :: IO Bool
test_mWhen_IO_False = do
    ref <- newIORef 'a'
    mWhen (return False) $ writeIORef ref 'A'
    val <- readIORef ref
    return $ val == 'a'


test_mUnless_IO_True :: IO Bool
test_mUnless_IO_True = do
    ref <- newIORef 'a'
    mUnless (return True) $ writeIORef ref 'A'
    val <- readIORef ref
    return $ val == 'a'


test_mUnless_IO_False :: IO Bool
test_mUnless_IO_False = do
    ref <- newIORef 'a'
    mUnless (return False) $ writeIORef ref 'A'
    val <- readIORef ref
    return $ val == 'A'
