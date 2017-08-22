# Junior   [![Build Status](https://secure.travis-ci.org/holoed/Junior.png?branch=master)](http://travis-ci.org/holoed/Junior)

Very Simple Functional Language

To build from Source type this commands at the prompt:

1)  cd src              <-- Source Directory
2)  ghci                <-- Haskell GHC Repl
3)  :load Main.hs       <-- Load the interpreter source into GHCI
4)  main                <-- This runs the Haskell Junior Interpreter (Code.jnr) and produces a bin.js compiler binary
5)  :q                  <-- Exit GHCI
6)  node                <-- Run Node JS
7)  .load bin.js        <-- Load the initial compiler binary into the NodeJS Repl
8)  main()              <-- This runs the compiler in the NodeJS Repl and produces a new compiler binary bin2.js
9)  .load bin2.js       <-- Load the second compiler binary into the NodeJS Repl
10) main("CodeV3.jnr")  <-- This runs the second compiler in the NodeJS repl compiling CodeV3.jnr into CodeV3.js
11) .exit               <-- Exit the NodeJS Repl
12) cd ..               <-- CD out of the source folder into the Junior folder
13) chmod +x jnr.sh     <-- Makes jnr.sh helper script executable
14) ./jnr.sh CodeV3.jnr <-- This runs the CodeV3.js compiler to compile itself and produce a CodeV3.js as a result.

To compile and run a sample:

1) /jnr.sh Sample.jnr             <-- Compiles the Sample.jnr file using the CodeV3 compiler and produces the binary Sample.js
2) cd src                         <-- Source Directory
3) node                           <-- Run NodeJS
4) require("./Sample.js").main()  <-- Executes the binary in the NodeJS Repl and returns 120 (factorial of 5)
