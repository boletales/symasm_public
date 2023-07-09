let fs = require("fs");
let libfast = require("./libfast");

let result = libfast.moderun(fs.readFileSync(process.argv[2]).toString(),fs.readFileSync(process.argv[3]).toString());
if(process.argv.length>3 && process.argv[4]=="-s"){
  process.stdout.write(result.strout);
  process.stderr.write(result.log[0] + "\n");
}else{
  process.stderr.write(result.log.join("\n"));
}