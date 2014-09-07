{-# LANGUAGE OverloadedStrings #-}
import Text.Email.Validate
import Network.API.Mandrill

main :: IO ()
main = case validate "foo@example.com" of
  Left err   -> print $ "Invalid email!" ++ show err
  Right addr -> runMandrill "SUPER-SECRET-TOKEN" $ do
    let msg = "<p>My Html</p>"
    res <- sendEmail (newTextMessage addr [addr] "Hello" msg)
    case res of
      MandrillSuccess k -> liftIO (print k)
      MandrillFailure f -> liftIO (print f)
