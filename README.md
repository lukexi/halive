* Halive
Live recompiler for Haskell

Use like
```halive <path/to/mymain.hs> <optional-include-dirs>```

Any time you change a file in the current directory or its subdirectories,
halive will recompile and rerun the ```main``` function in the file you gave it.

To keep alive values that aren't amenable to recompilation 
(such as GLFW or SDL's windows), use Chris Done's ```foreign-store``` library.

See a demonstration of this by running ```demo.sh``` 
and changing values in ```glfw.hs``` and ```Green.hs```.

(requires ```gl```, ```GLFW-b```, and ```foreign-store```)

Creating and deleting modules in the include path should 
work fine during a halive session.

Halive also works nicely with either batch-processing or run-loop type
programs — if the program finishes, it will be restarted on next save,
and if it's still running, it will be killed and restarted on save.

(To kill Halive during run-loop type programs, you may need to hold down Ctrl-C
to get GHC to recognize the double-Control-C-kill sequence.)

Note:
Executables based on halive must be built with the -dynamic flag for ghc
or else strange errors may occur when interfacing with libraries
(GHCi uses this flag for its executable as well).