var compiler = require('./CodeV4/Main.js')

var args = process.argv.slice(2)

compiler.main({ fst: args[0], snd: args[1] });
