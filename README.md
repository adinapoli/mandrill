[![Build Status](https://travis-ci.org/adinapoli/mandrill.svg?branch=master)](https://travis-ci.org/adinapoli/mandrill)
[![Coverage Status](https://img.shields.io/coveralls/adinapoli/mandrill.svg)](https://coveralls.io/r/adinapoli/mandrill)

# Haskell Client for the Mandrill JSON API

This module implement a low-level, 1:1 mapping API to
the [Mandrill](http://mandrillapp.com) transactional email service.

# Changelog

## Version 0.5.2.2

* Added support for QuickCheck-2.9 by relaxing its upper bound.

## Version 0.5.2.1

* Added `Functor`, `Foldable` and `Traversable` instances to `MandrillResponse` (Courtesy of @dredozubov)

## Version 0.5.2.0

* Added inbound calls (Courtesy of @mwotton)

## Version 0.5.1.0

* Relaxed the constraint on `aeson` to allow `0.11.0.0`.

## Version 0.5.0.0

* Changed the `MandrillHeaders` type synonym from `Value` to `Object`.

* Changed the `mmsg_metadata` and `mmdt_values` fields from `MandrillVars` to `Object`.

* Changed the `mmsg_global_merge_vars` and `mmvr_vars` fields from `[MandrillVars]` to `[MergeVar]`

* Added the `MergeVar` data type:

```haskell
data MergeVar = MergeVar {
      _mv_name    :: !Text
    , _mv_content :: Value
    }
```

* Removed the `MandrillVars` type synonym.

## Version 0.4.0.0

* Modified the `Base64ByteString` type to accept another constructor. This
  allows the user to pass already-encoded Base64 strings which might be coming
  upstream.

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

  - [Senders call](https://mandrillapp.com/api/docs/senders.JSON.html)
    + verify-domain.json

  - [Inbound call](https://mandrillapp.com/api/docs/inbound.JSON.html)
    + add-route.json
    + add-domain.json

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
