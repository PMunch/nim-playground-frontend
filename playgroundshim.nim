import jester, json
import osproc

routes:
  get "/":
    resp "Hello world"
  post "/compile":
    try:
      echo request.body
      var push = parseJson(request.body)
      writeFile("/tmp/nimcode.nim", push["code"].getStr())
      let (output, exitCode) = execCmdEx "cd /tmp && nim " & $push["compileTarget"] & " nimcode.nim"
      if exitCode == 0:
        let response = %*{"compileLog": output, "log": execCmdEx("cd /tmp && ./nimcode")[0]}
        resp Http200, [("Acces-Control-Allow-Origin", "*")], $response
      else:
        resp %*{"compileLog": output, "log":""}
    except:
      resp "Whoopsie, your JSON seems to be malformed"
