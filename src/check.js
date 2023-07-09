let fs = require("fs");
let path = require("path");
let libfast = require("./libfast");
let root = path.join(path.dirname(process.argv[1]),"../data");
let questions = fs.readdirSync(root).filter(p => fs.statSync(path.join(root, p)).isDirectory());
questions.forEach(q => {
    let files = fs.readdirSync(path.join(root,q));
    let nummode = files.includes("nummode");
    let code = fs.readFileSync(path.join(root,q,"cmds.sasm"), "utf-8");
    let count = 0;
    let ac    = 0;
    for(let i=1; files.includes("in"+i+".txt") && files.includes("out"+i+".txt"); i++){
        count++;
        let tin  = fs.readFileSync(path.join(root,q,"in"+i+".txt" ),"utf-8");
        let tout = fs.readFileSync(path.join(root,q,"out"+i+".txt"),"utf-8");
        let result = libfast.moderun(code, tin, nummode);
        if((nummode && result.output.join(" ") == tout) || (!nummode && result.strout == tout)){
            ac++;
        }else{
        }
    }
    if(count == ac){
        console.log(q + " ".repeat(15-q.length) + "[AC] " + ac + "/" + count);
    }else{
        console.log(q + " ".repeat(15-q.length) + "[WA] " + ac + "/" + count);
    }
});
