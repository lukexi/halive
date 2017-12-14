# Revision history for halive
## 0.1.4.  -- 2017-03-23
* Remove extraneous argument from compileExpression
* Fix change detection for editors that delete and recreate files rather than modifying them
* Ignores emacs flycheck/flymake and before-save files (Schell Scivally)
* Add ability to pass just file contents rather than an actual file
* Add ability to turn off language features (e.g. NoImplicitPrelude)
* Add liveExpression

## 0.1.3.  -- 2017-02-24
* Allows Halive to be used in a nix environment (Jude Taylor)

## 0.1.2.  -- 2017-01-02
* Restores ability for Halive to watch surrounding files in a dir
	(and lays groundwork to allow configuration of which filetypes are watched)

## 0.1.1.  -- 2016-12-28
* GHC8 support
* Windows support
* Only restarts your program once all type errors are fixed.
* Halive-as-a-library, aka "SubHalive"
* halive exe now uses SubHalive as core.
* Add persistState utility to store/restore state in a State monad, for easily preserving program state across recompilations
* Prioritize stack's "local-pkg-db:" over "snapshot-pkg-db:" to allow overriding packages just as stack does
* Switch to SDL for demo
* Add the demo as a test-suite to manage its dependencies

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
