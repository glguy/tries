0.3.2
-----

* Add a `trieAlter` method, and use it to speed up `alter`, `insert` and
  `delete`.
* Make the `Show` instance user friendly.
* Add support for GHC versions up to 9.4.
* Fix builds on GHC 7.8, and with old `containers` versions.
* Stop pretending to support GHC 7.4 and 7.6. These have not actually worked in
  some time, and it is hard to test on them.

0.3.1
-----

* Improved strictness
* GHC 8.4 compatibility

0.3.0.2
-------

* GHC 8 compatibility
