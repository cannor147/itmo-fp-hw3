{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase  #-}

module HW3.Action
  ( HiPermission(..)
  , HIO(..)
  , PermissionException(..)
  ) where

import           Control.Exception    (Exception, throwIO)
import           Control.Monad.Reader (ReaderT (..))
import           Data.ByteString      (readFile, writeFile)
import           Data.Functor         ((<&>))
import           Data.Sequence        (fromList)
import           Data.Set             (Set, member)
import           Data.Text            (pack)
import           Data.Text.Encoding   (decodeUtf8')
import           HW3.Base
import           System.Directory     (createDirectory, doesFileExist,
                                       getCurrentDirectory, listDirectory,
                                       setCurrentDirectory)

-- | Permissions for runnable expressions.
data HiPermission = AllowRead | AllowWrite | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Permission exception if there were actions without permissions.
newtype PermissionException = PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

-- | Monad for evaluating hi IO actions.
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

doAction :: HiPermission -> IO a -> HIO a
doAction permission action = HIO (\permissions -> if member permission permissions
                                                  then action
                                                  else throwIO $ PermissionRequired permission)

doAction' :: HiPermission -> IO a -> HIO HiValue
doAction' permission action = doAction permission $ action >> pure HiValueNull

pathToHiValue :: FilePath -> HiValue
pathToHiValue = HiValueString . pack

instance HiMonad HIO where
  runAction (HiActionWrite path bytes) = doAction' AllowWrite $ Data.ByteString.writeFile path bytes
  runAction (HiActionMkDir path)       = doAction' AllowWrite $ createDirectory path
  runAction (HiActionChDir path)       = doAction' AllowRead $ setCurrentDirectory path
  runAction HiActionCwd                = doAction AllowRead $ pathToHiValue <$> getCurrentDirectory
  runAction (HiActionRead  path)       = doAction AllowRead $ doesFileExist ("./" ++ path) >>= \case
    False -> listDirectory path <&> HiValueList . fromList . (pathToHiValue <$>)
    True  -> Data.ByteString.readFile ("./" ++ path) <&> transformBytes
    where
      transformBytes bytes = either (const $ HiValueBytes bytes) HiValueString . decodeUtf8' $ bytes
