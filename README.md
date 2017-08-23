# Junior   [![Build Status](https://secure.travis-ci.org/holoed/Junior.png?branch=master)](http://travis-ci.org/holoed/Junior)

Very Simple Functional Language

To build from Source type these commands at the prompt:

1) cabal install hspec
2) ./buildAndRunTests.sh
3) cabal run Junior
4)  node                
5)  .load js/CodeV2.js   
6)  main()          
7)  .load js/CodeV3.js      
8) main({ fst: "jnr/CodeV3.jnr", snd: "js/CodeV4.js" })   
9) .exit               
10) chmod +x jnr.sh    
11) ./jnr.sh jnr/CodeV3.jnr js/CodeV5.js

To compile and run a sample:

1) ./jnr.sh jnr/Sample.jnr js/Sample.js             
3) node                           <-- Run NodeJS
4) require("./js/Sample.js").main()  <-- Executes the binary in the NodeJS Repl and returns 120 (factorial of 5)
