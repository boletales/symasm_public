function checkAddress(a){
  if(a<0 || 1048576<a) throw new Error("address out of range ("+a+")");
}

let com_n = {
  "_" : ((m,a)=> {})
}

let com_v = {
  "!" : ((m,a)=> {m.output.push(a[0]);})
}

let com_a = {
  "?" : ((m,a)=> {checkAddress(a[0]); if(m.input.length < 0) m.mem[a[0]] = 0 ; else m.mem[a[0]] = m.input.shift() | 0;})
}

let com_av = {
  ":" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = ( a[1]) | 0;}),
  "~" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (~a[1]) | 0;})
}

let com_avv = {
  "+" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] +  a[2]) | 0;}),
  "-" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] -  a[2]) | 0;}),
  "*" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] *  a[2]) | 0;}),
  "/" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] /  a[2]) | 0;}),
  "%" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] %  a[2]) | 0;}),
  "&" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] &  a[2]) | 0;}),
  "|" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] |  a[2]) | 0;}),
  "^" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] ^  a[2]) | 0;}),
  "}" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] >> a[2]) | 0;}),
  "{" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] << a[2]) | 0;}),
  ">" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] >  a[2]) | 0;}),
  "<" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] <  a[2]) | 0;}),
  "=" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] == a[2]) | 0;})
}

function countAtmark(str, isAddr){
  let atm = 0;
  while(str.length > 0 && str[0] == "@"){
    atm ++;
    str = str.slice(1);
  }
  let i = parseInt(str);
  if(isAddr){
    if(i == NaN || atm == 0){
      throw new Error("invalid address");
    }else{
      return [atm-1, i];
    }
  }else{
    if(i == NaN){
      throw new Error("invalid value");
    }else{
      return [atm, i];
    }
  }
}

function evalref(mem, val){
  let r = val[0];
  let v = val[1];
  for(;0<r;r--){
    checkAddress(v);
    v = mem[v];
  }
  return v;
}

function run(str, input=[], steplimit=10000000){
  let machine = {
      input: input,
      output: [],
      time: 0,
      mem: new Int32Array(1048576).fill(0)
    };
  let log = [];
  let l = 0;
  let code;
  try {
    code = str.split("\n").map(l=>l.replace(/\s+/g," ").replace(/\/\/.*$/g,"").replace(/ $/g,"")).map((line, i)=>{
      l = i;
      let args = line.split(" ");
      if(Object.keys(com_n).includes(args[0])){
        if(args.length != 1) throw new Error("wrong arity");
        return {com: com_n[args[0]], args:[]};
      }else if(Object.keys(com_v).includes(args[0])){
        if(args.length != 2) throw new Error("wrong arity");
        return {com: com_v[args[0]], args:[countAtmark(args[1], false)]};
      }else if(Object.keys(com_a).includes(args[0])){
        if(args.length != 2) throw new Error("wrong arity");
        return {com: com_a[args[0]], args:[countAtmark(args[1], true)]};
      }else if(Object.keys(com_av).includes(args[0])){
        if(args.length != 3) throw new Error("wrong arity");
        return {com: com_av[args[0]], args:[countAtmark(args[1], true), countAtmark(args[2], false)]};
      }else if(Object.keys(com_avv).includes(args[0])){
        if(args.length != 4) throw new Error("wrong arity");
        return {com: com_avv[args[0]], args:[countAtmark(args[1], true), countAtmark(args[2], false), countAtmark(args[3], false)]};
      }else{
        throw new Error("wrong instruction");
      }
    });
  } catch (error) {
    log.push("Syntax Error at program["+l+"]: " + error.message);
    return {log:log, output:[], strout:""};
  }
  try {
    while(true){
      if(machine.mem[0] == code.length){
        break;
      }else if(machine.mem[0] < 0 || code.length < machine.mem[0]){
        throw new Error("program counter out of range (" + machine.mem[0]);
      }else if(steplimit < machine.time){
        throw new Error("step limit exceeded ("+steplimit+" ticks)");
      }else{
        let c = code[machine.mem[0]];
        //console.log(machine.mem[0], c.args.map(a=>evalref(machine.mem, a)));
        machine.time++;
        machine.mem[0]++;
        c.com(machine, c.args.map(a=>evalref(machine.mem, a)));
      }
    }

    log.push("Success!("+machine.time+" ticks)");
    log.push(JSON.stringify(machine.output));
    log.push(machine.output.map(c=>String.fromCodePoint(c)).join(""));
    return {log:log, output:machine.output, strout:machine.output.map(c=>String.fromCodePoint(c)).join("")};
  } catch (error) {
    log.push("Runtime Error at program["+machine.mem[0]+"]: " + error.message);
    log.push(JSON.stringify(machine.output));
    log.push(machine.output.map(c=>c>0?String.fromCodePoint(c):"�").join(""));
    return {log:log, output:machine.output, strout:machine.output.map(c=>c>0?String.fromCodePoint(c):"�").join("")};
  }
}


function moderun(code, input, isnummode=false){
  if(isnummode){
    return run(code, input.split(" ").map(c=>(i=>isNaN(i)?0:i)(parseInt(c))));
  }else{
    return run(code, input.split("").map(c=>c.codePointAt(0)));
  }
}


exports.moderun = moderun;