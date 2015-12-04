# Revision history for halive

## 0.1.0.8  -- TBD
* Windows support!
* Add the demo as a test-suite to manage its dependencies
* Prioritize stack's "local-pkg-db:" over "snapshot-pkg-db:" to allow overriding packages just as stack does
* Now runs a typechecker thread in the background, and only restarts your program once all type errors are fixed.

## 0.1.0.7  -- 2015-08-12
* Implement support for stack projects

## 0.1.0.6  -- 2015-08-07
* Remove system-filepath

## 0.1.0.5  -- 2015-06-29
* 7.8 compatibility fix

## 0.1.0.4  -- 2015-06-23
* Add Halive.Concurrent to help with killing threads when restarting a program

## 0.1.0.2/0.1.0.3  -- 2015-06-20
* Add command line argument support (Jonathan Geddes)
* Fix compilation on Windows, although Halive doesn't actually work yet

## 0.1.0.1  -- 2015-05-26
* Compilation fix

## 0.1.0.0  -- 2015-05-26

* First version.
