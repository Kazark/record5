-- | This is a simple approach to throwing errors in a way that provide
-- | straightforward messages to user. It keeps down the noise and keeps
-- | the equivocation "user error:" out of the message.
module Control.UserFacingError where

import Control.Exception (Exception, throwIO)

-- | In this application:
-- | * We do not need to handle exceptions, once thrown
-- | * We do not need to format them in multiple ways
-- | * We are only building a small example, so let's not overthink it
-- | Thus, I choose to simply newtype a string as my error type, which in a more
-- | robust scenario would not be ideal.
newtype UserFacingError = UserFacingError String

instance Exception UserFacingError

instance Show UserFacingError where
  show (UserFacingError s) = s

-- | We name this to conflict with Prelude's fail deliberately, to force
-- | ourselves to hide Prelude's fail, as we do not want to use it accidentally.
-- | It gives the loathsome "user error:" verbage; we do not want to yell at the
-- | user that they have made an error in that way! (I am sure it actually means
-- | "user-facing error", but even then that's useless noise.)
fail :: String -> IO a
fail = throwIO . UserFacingError
