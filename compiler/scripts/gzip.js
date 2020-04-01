const zlib = require('zlib');
const exec = require('util').promisify(require('child_process').exec);
const path = require('path');
const fs = require('fs');

process.on('uncaughtException', (err) => {
    throw err;
});

const runPath = process.cwd();

if(process.argv.length < 3) {
    console.log(`Usage: node ${path.relative(runPath, __filename)} <solc file path>`);
    process.exit(1);
}
const solc = path.join(runPath, process.argv[2])

if(!fs.existsSync(solc)) {
    console.log(`File ${solc} is not exist`);
    process.exit(2);
}

(async () => {
    const proc = await exec(`${solc} --version`);
    const ver = proc.stdout.toString().match(/[\d]{1,}\.[\d]{1,}\.[\d]{1,}/g)[0].trim();
    console.log(`Version: ${ver}`);
    
    const dst = path.join(runPath, `${path.parse(solc).name}_${ver.toString().replace(/\./gi, '_')}_${require('os').platform()}.gz`);
    if(fs.existsSync(dst)) {
        fs.unlinkSync(dst);
    }
    
    fs.createReadStream(solc).pipe(zlib.createGzip({level: 9})).pipe(fs.createWriteStream(dst)).on('close', () => {
        console.log(`File ${dst} was been successfully produced`);
    });

    const baseFile = path.join(runPath, 'solc.json');
    let base=null;
    if(fs.existsSync(baseFile)) {
        base = JSON.parse(fs.readFileSync(baseFile));
    } else {
        base = {solc: []};
    }
    if(!base.solc.includes(ver)) {
        base.solc.push(ver);
        fs.writeFileSync(baseFile,JSON.stringify(base));
        console.log(`${baseFile} was been updated`);
    }
})()