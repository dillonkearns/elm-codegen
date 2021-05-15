

/*

    elm-prefab Gen.elm --output=./dir
        -> compile the elm file and run it via the index.js runner

    elm-prefab Gen.elm --watch
        -> recompile and run when the file changes

    elm-prefab elm/json --output=./dir
        -> generate bindings for the elm/json package based on it's docs.json
        -> puts generated code in ./dir

    elm-prefab docs.json
        -> same as above, but from a local set of docs

*/

import * as commander from 'commander' // include commander in git clone of commander repo
import * as elm_compiler from 'node-elm-compiler'
import * as path from 'path';
import * as fs from 'fs';
import {XMLHttpRequest}  from './run/vendor/XMLHttpRequest';
import * as chokidar from 'chokidar';


async function run_generator(base:string, moduleName:string, elm_source:string) {
    // We have to stub this in the allow Elm the ability to make http requests.
    // @ts-ignore
    this.XMLHttpRequest = XMLHttpRequest
    eval(elm_source);

    const promise = new Promise((resolve, reject) => {
        // @ts-ignore
        const app = this.Elm[moduleName].init();
        if (app.ports.onSuccessSend) {
            app.ports.onSuccessSend.subscribe(resolve)
        }
        if (app.ports.onFailureSend) {
            app.ports.onFailureSend.subscribe(reject)
        }

    })
        .then((files:any) => {
            for (const file of files) {
                const fullpath = path.join(base, file.path)
                fs.mkdirSync(path.dirname(fullpath), { recursive: true })
                fs.writeFileSync(fullpath, file.contents)
            }
        })
        .then((_) => console.info("Success!"))
        .catch((reason) => console.error("Failure", reason))
    return promise
}

const elm_process_opts = { stdio: [null, 'ignore', 'inherit'] }
const program = new commander.Command();


function generate(elm_file: string, moduleName:string, target_dir: string, base: string){
    const data = elm_compiler.compileToStringSync([elm_file], {cwd: base, processOpts: elm_process_opts });
    if (data === "") {
        throw "Compiler error";
    }
    // @ts-ignore
    new run_generator(target_dir, moduleName, data.toString());
}

program
  .version('0.1.0')
  .arguments('<file>')
  .option('--watch', 'Watch the given file for changes and rerun the generator when a change is made.')
  .option('--cwd <dir>', 'Change the base directory for compiling your Elm generator')
  .option('--output', 'The directory where your generated files should go.')
  .option('--from-docs', 'The docs.json file to generate package bindings from.')
  .action((file, options, com) => {
    console.log("FILE:" , file)
    console.log(options)

    const cwd = options.cwd || "."
    const output = options.output || "generated"

    if (file.endsWith(".elm")) {
        const moduleName = path.parse(file).name

        if (options.watch) {

            generate(file, moduleName, output, cwd)
            chokidar.watch(path.join(cwd, "**", "*.elm"), {ignored: path.join(output, "**")} ).on('all', (event, path) => {
                console.log("\nFile changed, regenerating")
                generate(file, moduleName, output, cwd)
            });
        } else {
           generate(file, moduleName, output, cwd)
        }


    } else if (file.endsWith(".json")) {
        console.log("JS")
    } else if (file.split("/").length == 2) {
        console.log("Elm package!")
    }
  });

program.parse();