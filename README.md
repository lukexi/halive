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

Note:
Executables based on halive must be built with the -dynamic flag for ghc
or else strange errors may occur when interfacing with libraries
(GHCi uses this flag for its executable as well).