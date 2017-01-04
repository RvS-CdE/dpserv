dpserv
======
*work in progress*

RESTful HATEOAS document server, with on-the-fly pdf to text and html conversion, support for collections and applying different service limit controls.

**Goal**: Provide an easy-to-use API for downloading and synchronizing collections of public documents in PDF format.

Example client:
https://github.com/PieterjanMontens/dpserv_client

Current state
-------------

 * Serves PDF files
 * Provides HATEOAS browsing from the root URL up to each different document
 * Extracts text from PDF files, and can serve it in text and html
 * Applies basic rate limiting (see [config file](include/config.hrl) )

 
Technology
----------
 
 * Erlang/OTP
 * [Rebar3](https://www.rebar3.org/), [Cowboy2](https://github.com/ninenines/cowboy), 
  [Lager](http:s://github.com/basho/lager), [Jiffy](https://github.com/davisp/jiffy), [iso8601](https://github.com/erlsci/iso8601)
 * Integrates with Apache TIKA for document conversion

TODO
----
A lot.


Build
-----

    $ rebar3 compile
