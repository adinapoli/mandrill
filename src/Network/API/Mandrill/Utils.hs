
module Network.API.Mandrill.Utils where

import Data.Char

--------------------------------------------------------------------------------
-- | Turns camelCase strings into suitable dashified ones.
-- >>> modRejectReason "TestModeLimit"
--     "test-mode-limit"
modRejectReason :: String -> String
modRejectReason [] = []
modRejectReason (x:xs) = toLower x : go xs
  where
    go [] = []
    go (y:ys) = case isUpper y of
      False -> y : go ys
      True  -> '-' : toLower y : go ys
