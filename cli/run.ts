/*

    elm-codegen Gen.elm --output=./dir
        -> compile the elm file and run it via the index.js runner

    elm-codegen Gen.elm --watch
        -> recompile and run when the file changes

    elm-codegen install elm/json --output=./dir
        -> generate bindings for the elm/json package based on it's docs.json
        -> puts generated code in ./dir

    elm-codegen install docs.json
        -> same as above, but from a local set of docs

    elm-codegen install Module.elm
        -> same as above, but from a local elm file itself

*/

import * as elm_compiler from "node-elm-compiler"
import * as https from "https"
import * as path from "path"
import * as fs from "fs"
import { XMLHttpRequest } from "./run/vendor/XMLHttpRequest"
import * as Chokidar from "chokidar"
import chalk from "chalk"
import templates from "./templates"
const gen_package = require("./gen-package")

// We have to stub this in the allow Elm the ability to make http requests.
// @ts-ignore
globalThis["XMLHttpRequest"] = XMLHttpRequest.XMLHttpRequest

const currentVersion = require("../package.json").version

async function httpsGetJson(url: string): Promise<any> {
  return new Promise((resolve, reject) => {
    https
      .get(url, (res) => {
        const chunks: Buffer[] = []

        res.on("data", (chunk: Buffer) => {
          chunks.push(chunk)
        })

        res.on("end", () => {
          const body = Buffer.concat(chunks)
          resolve(JSON.parse(body.toString()))
        })
      })
      .on("error", reject)
  })
}

async function run_generator(output_dir: string, moduleName: string, elm_source: string, flags: any) {
  eval(elm_source)

  const promise = new Promise<{ path: string; contents: string }[]>((resolve, reject) => {
    // @ts-ignore
    if (!(moduleName in this.Elm)) {
      console.log(
        chalk.cyan(
          // @ts-ignore
          `Module ${moduleName} not found in compile Elm code. Available modules are: ${JSON.stringify(this.Elm)}`
        )
      )
      return 1
    }

    // @ts-ignore
    const app = this.Elm[moduleName].init({ flags: flags })
    if (app.ports.onSuccessSend) {
      app.ports.onSuccessSend.subscribe(resolve)
    }
    if (app.ports.onInfoSend) {
      app.ports.onInfoSend.subscribe((info: string) => console.log(info))
    }
    if (app.ports.onFailureSend) {
      app.ports.onFailureSend.subscribe(reject)
    }
  })

  try {
    const files = await promise
    for (const file of files) {
      const fullpath = path.join(output_dir, file.path)
      fs.mkdirSync(path.dirname(fullpath), { recursive: true })
      fs.writeFileSync(fullpath, file.contents)
    }
    if (files.length == 1) {
      console.log(format_block([`${chalk.cyan(output_dir + path.sep)}${chalk.yellow(files[0].path)} was generated!`]))
    } else {
      console.log(format_block([`${chalk.yellow(files.length)} files generated in ${chalk.cyan(output_dir)}!`]))
    }
  } catch (errors: { title: string; description: string }[] | any) {
    let formatted = ""

    if (!!errors[Symbol.iterator]) {
      for (const err of errors) {
        formatted = formatted + format_title(err.title) + "\n\n" + err.description + "\n"
      }
    } else {
      if (JSON.stringify(errors.message).includes("https://github.com/elm/core/blob/1.0.0/hints/2.md")) {
        formatted = `Problem with the flags given to your Elm application on initialization.\n\n\nAdd the ${chalk.cyan(
          "--debug"
        )} to see more details!` // Assuming this is an Elm init error.
      } else {
        formatted = chalk.cyan(errors.message)
      }
    }
    console.error(formatted)
    return 1
  }
}

function generate(debug: boolean, elm_file: string, moduleName: string, output_dir: string, cwd: string, flags: any) {
  try {
    const data = elm_compiler.compileToStringSync([elm_file], {
      cwd: cwd,
      optimize: !debug,
      processOpts: { stdio: [null, null, "inherit"] },
    })

    // @ts-ignore
    return new run_generator(output_dir, moduleName, data.toString(), flags)
  } catch (error: unknown) {
    // This is generally an elm make error from the elm_compiler
    console.log(error)
  }
}

function getFilesWithin(dir: string, endsWith: string) {
  let files: string[] = []
  fs.readdirSync(dir).forEach((file) => {
    const absolute = path.join(dir, file)
    if (fs.statSync(absolute).isDirectory()) {
      files = files.concat(getFilesWithin(absolute, endsWith))
    } else {
      if (absolute.endsWith(endsWith)) {
        files.push(absolute)
      }
    }
  })
  return files
}
const docs_generator = { cwd: "cli/gen-package", file: "src/Generate.elm", moduleName: "Generate" }

function format_title(title: string): string {
  const tail = "-".repeat(80 - (title.length + 2))
  return chalk.cyan("--" + title.toUpperCase() + tail)
}

function format_block(content: string[]) {
  return "\n    " + content.join("\n    ") + "\n"
}

async function run_package_generator(output: string, flags: any) {
  const promise = new Promise((resolve, reject) => {
    // @ts-ignore
    const app = gen_package.Elm.Generate.init({ flags: flags })
    if (app.ports.onSuccessSend) {
      app.ports.onSuccessSend.subscribe(resolve)
    }
    if (app.ports.onInfoSend) {
      app.ports.onInfoSend.subscribe((info: string) => console.log(info))
    }
    if (app.ports.onFailureSend) {
      app.ports.onFailureSend.subscribe(reject)
    }
  })
    .then((files: any) => {
      for (const file of files) {
        const fullpath = path.join(output, file.path)
        fs.mkdirSync(path.dirname(fullpath), { recursive: true })
        fs.writeFileSync(fullpath, file.contents)
      }
    })
    .catch((reason) => {
      console.error(format_title(reason.title), "\n\n" + reason.description + "\n")
    })
  return promise
}

// INSTALL
//   Install bindings for a package
async function install_package(
  pkg: string,
  install_dir: string,
  version: string | null,
  codeGenJson: CodeGenJson
): Promise<CodeGenJson> {
  if (version == null) {
    const search = await httpsGetJson("https://elm-package-cache-psi.vercel.app/search.json")
    for (let found of search) {
      if (found.name == pkg) {
        version = found.version
        break
      }
    }
    if (version == null) {
      console.log(format_block([`No package found for ${chalk.yellow(pkg)}`]))
      process.exit(1)
    }
  }
  const docs = await httpsGetJson(`https://elm-package-cache-psi.vercel.app/packages/${pkg}/${version}/docs.json`)

  // let codeGenJson = getCodeGenJson(install_dir)

  if (codeGenJson.version != currentVersion) {
    console.log(
      chalk.cyan("elm.codegen.json") +
        " says you are on version " +
        chalk.yellow(codeGenJson.version) +
        `, but you're running version ` +
        chalk.yellow(currentVersion)
    )
    process.exit(1)
  }
  if (codeGenJson.dependencies && codeGenJson.dependencies.packages && pkg in codeGenJson.dependencies.packages) {
    console.log(chalk.yellow(pkg) + ` is already installed!`)
    if (codeGenJson.dependencies.packages[pkg] != version) {
      console.log(
        `If you want to change versions, adjust ` +
          chalk.cyan("elm.codegen.json") +
          " and run " +
          chalk.cyan("elm-codegen install")
      )
    }
    process.exit(1)
  }

  try {
    run_package_generator(install_dir, { docs: docs })
    codeGenJson.dependencies.packages[pkg] = version
    // fs.writeFileSync(path.join(install_dir, "elm.codegen.json"), codeGenJsonToString(codeGenJson))
    return codeGenJson
  } catch (error: unknown) {
    console.log(`There was an issue generating docs for ${pkg}`)
    // @ts-ignore
    console.log(format_block([error]))
    process.exit(1)
  }
}

type CodeGenJson = {
  version: string
  dependencies: { packages: { [key: string]: string }; local: string[] }
}

function getCodeGenJsonDir(): string {
  if (fs.existsSync("elm.codegen.json")) {
    return "."
  } else if (fs.existsSync("codegen/elm.codegen.json")) {
    return "codegen"
  }

  console.log(
    format_block([
      "Looks like there's no " + chalk.yellow("elm.codegen.json") + ".",
      "Run " + chalk.cyan("elm-codegen init") + " to generate one!",
    ])
  )
  process.exit(1)
}

function getCodeGenJson(cwd: string): CodeGenJson {
  let stringContents = ""
  try {
    stringContents = fs.readFileSync(path.join(cwd, "elm.codegen.json")).toString()
  } catch (error) {
    console.log(
      format_block([
        "Looks like there's no " + chalk.yellow("elm.codegen.json") + ".",
        "Run " + chalk.cyan("elm-codegen init") + " to generate one!",
      ])
    )
    process.exit(1)
  }
  return json2CodeGenConfig(stringContents)
}

function json2CodeGenConfig(stringJson: string): CodeGenJson {
  try {
    let codeGenJson = JSON.parse(stringJson)
    return {
      version: codeGenJson["elm-codegen-version"],
      dependencies: { packages: codeGenJson["codegen-helpers"].packages, local: codeGenJson["codegen-helpers"].local },
    }
  } catch (exception_var) {
    // TODO: convert this exception to a more useful error message
    console.log(format_block(["Looks like there's an issue with " + chalk.yellow("elm.codegen.json") + "."]))
    process.exit(1)
  }
}

function codeGenJsonToString(codeGen: CodeGenJson): string {
  let obj: any = {}
  obj["elm-codegen-version"] = codeGen.version
  obj["codegen-helpers"] = codeGen.dependencies
  return JSON.stringify(obj, null, 4)
}

function codeGenJsonDefault(): CodeGenJson {
  let codeGenJson = JSON.parse(templates.init.elmCodegenJson())
  codeGenJson["elm-codegen-version"] = currentVersion

  return json2CodeGenConfig(JSON.stringify(codeGenJson))
}

// INIT
//    Start a new elm-codegen project
//    Generates some files and installs `core`
export async function init(desiredInstallDir: string | null) {
  const install_dir = desiredInstallDir || "codegen"

  const base = path.join(".", install_dir)
  // create folder
  if (fs.existsSync(base)) {
    console.log(format_block(["Looks like there's already a " + chalk.cyan(install_dir) + " folder."]))
    process.exit(1)
  }
  if (fs.existsSync(path.join(base, "elm.codegen.json"))) {
    console.log(
      format_block(["Looks like there's already a " + chalk.cyan(path.join(base, "elm.codegen.json")) + " file."])
    )
    process.exit(1)
  }

  const codeGenJson = codeGenJsonDefault()

  fs.mkdirSync(base)
  fs.mkdirSync(path.join(base, "Gen"))
  fs.mkdirSync(path.join(base, "Gen", "CodeGen"))
  fs.mkdirSync(path.join(base, "helpers"))

  fs.writeFileSync(path.join(base, "elm.json"), templates.init.elmJson())
  fs.writeFileSync(path.join(base, "Generate.elm"), templates.init.starter())
  fs.writeFileSync(path.join(base, "Gen", "CodeGen", "Generate.elm"), templates.init.codegenProgram())
  fs.writeFileSync(path.join(base, "helpers", "Helper.elm"), templates.init.helper())
  const updatedCodeGenJson = await install_package("elm/core", install_dir, null, codeGenJson)

  let helperPath = path.join(base, "helpers") + path.sep
  // install local helpers
  let elmSources: string[] = []
  getFilesWithin(helperPath, ".elm").forEach((elmPath) => {
    elmSources.push(fs.readFileSync(elmPath).toString())
  })
  run_package_generator(install_dir, { elmSource: elmSources })
  updatedCodeGenJson.dependencies.local.push(helperPath)

  fs.writeFileSync(path.join(base, "elm.codegen.json"), codeGenJsonToString(updatedCodeGenJson))

  console.log(
    format_block([
      "Welcome to " + chalk.yellow("elm-codegen") + "!",
      "",
      "I've created the " + chalk.cyan(install_dir) + " folder and added some files.",
      chalk.cyan(path.join(base, "Generate.elm")) + " is a good place to start to see how everything works!",
      "",
      "Run your generator by running " + chalk.yellow("elm-codegen run"),
    ])
  )
}

async function reinstall_everything(install_dir: string, codeGenJson: CodeGenJson) {
  console.log("Installing dependencies from " + chalk.yellow("elm.codegen.json"))

  const emptyCodeGenJson = codeGenJsonDefault()

  for (let [key, version] of Object.entries(codeGenJson.dependencies.packages)) {
    // `version` is a string
    // install_package returns a new CodeGenJson,
    // but we already know it should be exactly like the one we have

    // @ts-ignore
    await install_package(key, install_dir, version, emptyCodeGenJson)
  }

  const genFolderPath = path.join(install_dir, "Gen", "CodeGen")
  fs.mkdirSync(genFolderPath, { recursive: true })
  fs.writeFileSync(path.join(genFolderPath, "Generate.elm"), templates.init.codegenProgram())
  const elmSources = []
  for (const item of codeGenJson.dependencies.local) {
    console.log("Installing " + item)
    if (item.endsWith(".json")) {
      console.log("From json " + item)
      let docs = JSON.parse(fs.readFileSync(item).toString())
      run_package_generator(install_dir, { docs: docs })
    } else if (item.endsWith(".elm")) {
      elmSources.push(fs.readFileSync(item).toString())
    } else if (item.endsWith(path.sep)) {
      if (fs.existsSync(item)) {
        getFilesWithin(item, ".elm").forEach((elmPath) => {
          elmSources.push(fs.readFileSync(elmPath).toString())
        })
      } else {
        console.log(
          `\n${chalk.yellow(item)} is listed in your elm-codegen.json but doesn't exist!\nMaybe it should be removed?\n`
        )
      }
    }
  }
  if (elmSources.length > 0) {
    run_package_generator(install_dir, { elmSource: elmSources })
  }
  console.log(chalk.green("Success!"))
}

function clear(dir: string) {
  fs.readdir(dir, (err, files) => {
    if (err) throw err
    for (const file of files) {
      fs.unlink(path.join(dir, file), (err) => {
        if (err) throw err
      })
    }
  })
}

function isLocal(pkg: string) {
  return pkg.endsWith(".json") || pkg.endsWith(path.sep) || pkg.endsWith(".elm")
}

function copyHelpers(codeGenJson: CodeGenJson, options: Options) {
  // create output directory if it doesn't exist
  fs.mkdirSync(options.output, { recursive: true })
  // copy over all local dependencies
  for (const item of codeGenJson.dependencies.local) {
    if (item.endsWith(".elm")) {
      fs.writeFileSync(path.join(options.output, item), fs.readFileSync(item).toString())
    } else if (item.endsWith(path.sep)) {
      if (fs.existsSync(item)) {
        getFilesWithin(item, ".elm").forEach((elmPath) => {
          const relative = path.relative(item, elmPath)
          fs.writeFileSync(path.join(options.output, relative), fs.readFileSync(elmPath).toString())
        })
      }
    }
  }
}

export async function run_install(pkg: string, version: string | null) {
  const install_dir = getCodeGenJsonDir()
  let codeGenJson = getCodeGenJson(install_dir)
  const codeGenJsonPath = path.join(install_dir, "elm.codegen.json")
  if (!!pkg) {
    // Is it already installed?
    // If it is and they want a reinstall, they should run `elm-codegen install`
    if (isLocal(pkg) && pkg in codeGenJson.dependencies.local) {
      console.log(
        format_block([
          chalk.cyan(pkg) + " is already installed!",
          "Run " + chalk.yellow("elm-codegen install") + " if you want to refresh the helper files.",
        ])
      )
      process.exit(1)
    }

    if (pkg.endsWith(".json")) {
      //
      // Install local docs file
      console.log(format_block(["Adding " + chalk.cyan(pkg) + " to local dependencies and installing."]))
      let docs = JSON.parse(fs.readFileSync(pkg).toString())
      run_package_generator(install_dir, { docs: docs })
      codeGenJson.dependencies.local.push(pkg)
      fs.writeFileSync(codeGenJsonPath, codeGenJsonToString(codeGenJson))
    } else if (pkg.endsWith(path.sep)) {
      //
      // Install all files within the pkg directory
      console.log(format_block(["Adding the " + chalk.cyan(pkg) + " directory to local dependencies and installing."]))
      let elmSources: string[] = []
      getFilesWithin(pkg, ".elm").forEach((elmPath) => {
        elmSources.push(fs.readFileSync(elmPath).toString())
      })

      run_package_generator(install_dir, { elmSource: elmSources })
      codeGenJson.dependencies.local.push(pkg)
      fs.writeFileSync(codeGenJsonPath, codeGenJsonToString(codeGenJson))
    } else if (pkg.endsWith(".elm")) {
      //
      // Install local elm file
      run_package_generator(install_dir, { elmSource: [fs.readFileSync(pkg).toString()] })
      codeGenJson.dependencies.local.push(pkg)
      fs.writeFileSync(codeGenJsonPath, codeGenJsonToString(codeGenJson))
    } else {
      //
      // Install from elm package
      console.log("Installing " + chalk.cyan(pkg) + " in " + chalk.yellow(install_dir))
      const updatedCodeGenJson = await install_package(pkg, install_dir, version, codeGenJson)
      fs.writeFileSync(codeGenJsonPath, codeGenJsonToString(updatedCodeGenJson))
      console.log(chalk.green("Success!"))
    }
  } else {
    // elm-codegen install
    // means reinstall all packages
    reinstall_everything(install_dir, codeGenJson)
  }
}

export type Options = {
  debug: boolean
  output: string
  flags: unknown
  cwd: string | null
}

export async function run(elmFile: string, options: Options) {
  const moduleName = path.parse(elmFile).name
  const install_dir = getCodeGenJsonDir()
  let codeGenJson = getCodeGenJson(install_dir)
  copyHelpers(codeGenJson, options)
  generate(options.debug, elmFile, moduleName, options.output, options.cwd || ".", options.flags)
}

export type CliOptions = {
  debug: boolean
  output: string
  flagsFrom: string | null
  flags: string | null
  watch: boolean
}

function parseJSONSafe(hopefullyJsonString: string) {
  try {
    return JSON.parse(hopefullyJsonString)
  } catch (parsingError) {
    return null
  }
}

export async function run_generation_from_cli(desiredElmFile: string | null, options: CliOptions) {
  let elmFile = "Generate.elm"
  let cwd = "./codegen"

  if (desiredElmFile != null) {
    cwd = path.dirname(desiredElmFile)

    while (!fs.existsSync(path.join(cwd, "elm.json"))) {
      cwd = path.dirname(cwd)

      if (cwd == ".") {
        break
      }

      const parsed = path.parse(cwd)
      if (parsed.root == parsed.dir) {
        // Avoid infinite recursion
        break
      }
    }

    elmFile = path.relative(cwd, desiredElmFile)
  }
  let fullSourcePath = path.join(cwd, elmFile)
  let output = options.output

  if (!fs.existsSync(fullSourcePath)) {
    console.log(
      format_block([
        "I wasn't able to find " + chalk.yellow(fullSourcePath) + ".",
        "Have you set up a project using " + chalk.cyan("elm-codegen init") + "?",
      ])
    )
    process.exit(1)
  }

  // prepare flags
  let flags: any | null = null
  if (options.flagsFrom) {
    if (!fs.existsSync(options.flagsFrom)) {
      if (path.isAbsolute(options.flagsFrom)) {
        console.log(
          format_block([
            chalk.cyan("elm-codegen") + " was called with " + chalk.cyan("--flags-from"),
            chalk.yellow(options.flagsFrom),
            "but that file doesn't exist.",
          ])
        )
      } else {
        console.log(
          format_block([
            chalk.cyan("elm-codegen") + " was called with " + chalk.cyan("--flags-from"),
            chalk.yellow(options.flagsFrom),
            "I looked in " + chalk.cyan(process.cwd()) + " but wasn't able to find anything.",
            "Is there a typo in the path?",
          ])
        )
      }

      process.exit(1)
    }
    flags = fs.readFileSync(options.flagsFrom).toString()

    if (options.flagsFrom.endsWith(".json")) {
      const parsed = parseJSONSafe(flags)
      if (parsed == null && flags.trim().toLowerCase() != "null") {
        console.log(
          format_block([
            chalk.cyan("elm-codegen") + " was called with " + chalk.cyan("--flags-from"),
            chalk.yellow(options.flagsFrom) + " which has a .json extension, but I wasn't able to parse it as JSON.",
            "Is it valid JSON?",
          ])
        )
        process.exit(1)
      }
      flags = parsed
    }
  } else if (options.flags) {
    flags = parseJSONSafe(options.flags)
  }

  const moduleName = path.parse(elmFile).name

  // Copy locally installed helpers
  const install_dir = getCodeGenJsonDir()
  let codeGenJson = getCodeGenJson(install_dir)
  copyHelpers(codeGenJson, {
    debug: options.debug,
    output: options.output,
    flags: flags,
    cwd: null,
  })

  if (options.watch) {
    //         clear(output)

    generate(options.debug, elmFile, moduleName, output, cwd, flags)
    Chokidar.watch(path.join(cwd, "**", "*.elm"), { ignored: path.join(output, "**") }).on("all", (event, path) => {
      if (event === "change") {
        console.log(`\n${path} changed, regenerating`)
        generate(options.debug, elmFile, moduleName, output, cwd, flags)
      }
    })
  } else {
    //         skipping clearing files because in my test case it was failing with permission denied all the time.
    //         clear(output)
    generate(options.debug, elmFile, moduleName, output, cwd, flags)
  }
}
