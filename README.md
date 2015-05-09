```
██╗  ██╗ █████╗ ██╗     ██╗██╗   ██╗███████╗
██║  ██║██╔══██╗██║     ██║██║   ██║██╔════╝
███████║███████║██║     ██║██║   ██║█████╗  
██╔══██║██╔══██║██║     ██║╚██╗ ██╔╝██╔══╝  
██║  ██║██║  ██║███████╗██║ ╚████╔╝ ███████╗
╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═══╝  ╚══════╝
```
Live recompiler for Haskell

![Halive Demo](http://lukexi.github.io/HaliveDemo.gif)

Halive uses the GHC API to instantly recompile and reload your code any time you change it.

Usage:
`cabal install`

and then

`halive <path/to/mymain.hs> <extra-include-dirs>`

Any time you change a file in the current directory or its subdirectories,
halive will recompile and rerun the `main` function in the file you gave it.

See a demo by running `demo.sh` 
and changing values in `glfw.hs` and `Green.hs`
(requires `gl`, `GLFW-b`, and `foreign-store`).

Keeping values alive
--------------------

To keep state alive, import `Halive.Utils` and wrap
your value in `reacquire` along with a unique identifier, like:

`win <- reacquire 0 (setupGLFW "HotGLFW" 640 480)`

to only create the resource the first time you run the program, and then
reuse it on subsequent recompilations.

You can see this in action in `test/glfw.hs`.

Thanks to Chris Done's 
[`foreign-store`](https://hackage.haskell.org/package/foreign-store) 
library for enabling this.

Notes
-----

Creating, updating, and deleting modules in the include path should 
work fine during a Halive session. 

Halive also supports Cabal sandboxes; 
if run within a directory containing a cabal.sandbox.config file it will
use the package database defined therein.

Halive also works nicely with either batch-processing or run-loop type
programs — if the program finishes, it will be restarted on next save,
and if it's still running, it will be killed and restarted on save.

To kill Halive during run-loop type programs, you may need to hold down Ctrl-C
to get GHC to recognize the double-Control-C-kill sequence.

[@lukexi](http://twitter.com/lukexi)