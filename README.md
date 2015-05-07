Use like
```halive mymain.hs```

Monitors the current directory and all subdirectories for changes
in any .hs files, and recompiles and reloads your main file 
(and its dependencies).


If you're depending on the Halive library to build 
your own live-recompiler rather than using the halive 
command line tool, you must use the '-dynamic' GHC flag
(either in cabal's ghc-options: for your executable 
or by passing it to ghc).