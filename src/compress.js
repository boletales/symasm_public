let fs = require("fs");
let path = require("path");
let libfast = require("./libfast");
let root = path.join(path.dirname(process.argv[1]),"../data");
let questions = fs.readdirSync(root).filter(p => fs.statSync(path.join(root, p)).isDirectory());
console.log(JSON.stringify(questions.map(q => {
    let files = fs.readdirSync(path.join(root,q));
    let nummode   = files.includes("nummode");
    let info      = fs.readFileSync(path.join(root,q,"info.txt"),"utf-8");
    let tests     = [];
    for(let i=1; files.includes("in"+i+".txt") && files.includes("out"+i+".txt"); i++){
        let tin  = fs.readFileSync(path.join(root,q,"in"+i+".txt" ),"utf-8");
        let tout = fs.readFileSync(path.join(root,q,"out"+i+".txt"),"utf-8");
        tests.push({input:tin, output:tout})
    }
    return {name:q, info, nummode, tests};
})));
