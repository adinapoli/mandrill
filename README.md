[![Build Status](https://travis-ci.org/adinapoli/mandrill.svg?branch=master)](https://travis-ci.org/adinapoli/mandrill)
[![Coverage Status](https://img.shields.io/coveralls/adinapoli/mandrill.svg)](https://coveralls.io/r/adinapoli/mandrill)

# Haskell Client for the Mandrill JSON API

This module implement a low-level, 1:1 mapping API to
the [Mandrill](http://mandrillapp.com) transactional email service.

# Example

This package was built with pragmatism and reuse in mind. This means
this API comes in two flavours: an IO-based and an handy monad transformer
which can be plugged in your stack of choice.
Example:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Text.Email.Validate
import Network.API.Mandrill

main :: IO ()
main = do
  case validate "foo@example.com" of
    Left err   -> print $ "Invalid email!" ++ show err
    Right addr -> runMandrill "MYTOKENHERE" $ do
      let msg = "<p>My Html</p>"
      res <- sendEmail (newTextMessage addr [addr] "Hello" msg)
      case res of
        MandrillSuccess k -> liftIO (print k)
        MandrillFailure f -> liftIO (print f)
```

# Supported API versions

* 1.0 (partially)
  - [Users call](https://mandrillapp.com/api/docs/users.JSON.html) - 100%
    + info.json
    + ~~ping.json~~ (as **doesn't return valid json!**)
    + ping2.json
    + senders.json
  - [Messages call](https://mandrillapp.com/api/docs/messages.JSON.html)
    + send.json

# Testing online API

To test the online API, first build the package with tests enabled:

```
cabal install --enable-tests
```

Then export an environment variable with your Mandrill **Test** token:

```
export MANDRILL_API_KEY="YOURKEYGOESHERE"
```

And finally execute the testsuite:

```
cabal test
```

# Contributions
This library scratches my own itches, but please fork away!
Pull requests are encouraged to implement the part of the API
you need.
