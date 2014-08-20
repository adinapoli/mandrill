[![Build Status](https://travis-ci.org/adinapoli/mandrill.svg?branch=master)](https://travis-ci.org/adinapoli/mandrill)

# Haskell Client for the Mandrill JSON API

This module implement a low-level, 1:1 mapping API to
the [Mandrill](http://mandrillapp.com) transactional email service.

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
