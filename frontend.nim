include karax / prelude
import karax / [vstyles, kdom, kajax]
import jsffi except `&`
import json
import sugar

import macros, strutils

macro createAliases(tag: string, body: untyped): untyped =
  result = newStmtList()
  for classes in body:
    assert classes.kind == nnkStrLit, "Input must be a list of string literals"
    let tmplName = newIdentNode(classes.strVal.multiReplace([(" ", ""), ("-", "")]))
    result.add quote do:
      macro `tmplName`(rest: varargs[untyped]): untyped =
        var
          exprs: seq[NimNode]
          classes = `classes`
        for argument in rest:
          if $argument[0] != "class":
            exprs.add argument
          else:
            classes = classes & " " & argument[1].strVal

        var ret = nnkCall.newTree(
          newIdentNode("buildHtml"),
          nnkCall.newTree(
            newIdentNode(`tag`)
          )
        )
        for expr in exprs:
          ret[1].add expr
        ret[1].add nnkExprEqExpr.newTree(
            newIdentNode("class"),
            newLit(classes)
          )
        ret

createAliases("button"):
  "main button"
  "other button"

createAliases("tdiv"):
  "headerbar"
  "mainarea"
  "base column"
  "grow content"
  "bar"
  "big editor"
  "small column"
  "optionsbar"

type
  CodeMirror = distinct Element
  OutputKind = enum
    Debug = "Debug", Output = "Output"
  Regex = ref object of RootObj
  CompilerTargets = enum ## Compiler Backend Targets
    cTarget = "c "
    cppTarget = "cpp "
    jsTarget = "js "
    objcTarget = "objc "
    nodejsTarget = "js -d:nodejs "
    checkTarget = "check "
  CompilerModes = enum ## Compiler Backend Modes
    debugMode = ""
    releaseMode = "-d:release "
    dangerReleaseMode = "-d:release -d:danger "
  GarbageCollectors = enum ## Garbage Collectors
    defaultGc = ""
    goGc = "--gc:go "
    boehmGc = "--gc:bohem "
    markAndSweepGc = "--gc:markAndSweep "
    noneGc = "--gc:none "
    regionsGc = "--gc:regions "
    destructorsGc = "--newruntime "
  StyleCheckings = enum ## Style Checks
    styleCheckOff = ""
    styleCheckHint = "--styleCheck:hint "
    styleCheckError = "--styleCheck:error "


proc newCodeMirror(element: Element, config: js): CodeMirror {.importcpp: "CodeMirror(@)".}
proc setValue(cm: CodeMirror, value: kstring) {.importcpp: "#.setValue(@)".}
proc getValue(cm: CodeMirror): kstring {.importcpp: "#.getValue()".}
proc setOption(cm: CodeMirror, key: kstring, value: js) {.importcpp: "#.setOption(@)".}
proc replaceSelection(cm: CodeMirror, value: kstring) {.importcpp: "#.replaceSelection(@)".}
proc replace(str: kstring, r: Regex, cmd: proc (x: kstring): kstring): kstring {.importcpp: "#.replace(@)".}
proc setHash(str: kstring) {.importcpp: "window.location.hash = #".}
proc sanitize(str: kstring): kstring {.importcpp: "DOMPurify.sanitize(#)".}
converter toRegex(r: kstring): Regex {.importcpp: "new RegExp(#, 'g')", noSideEffect.}

var
  output = Output
  myCodeMirror: CodeMirror
  outputText: array[Output.low..Output.high, string]
  runningCode = false
  awaitingShare = false
  loadedIx = ""
  showingTour = false
  loadedTour = ""
  codeCurrentSection = -1
  currentSection = 0
  totalSections = 0
  tourContent = buildHtml:
    text "Loading tour..."
  knownVersions = @["latest"]

proc switchOutput() =
  output = case output:
    of Debug: Output
    of Output: Debug

proc runCode() =
  runningCode = true
  kxi.redraw()
  proc cb(httpStatus: int, response: cstring) =
    runningCode = false
    if httpStatus == 200:
      let jsonResponse = parseJson($response)
      outputText[Output] = jsonResponse["log"].getStr.replace("\n", "<br/>")
      outputText[Debug] = $(jsonResponse["compileLog"].getStr)
      if outputText[Output] != "<br/>":
        output = Output
      else:
        output = Debug
    else:
      outputText[Debug] = "No reply from server:<br/>" & $httpStatus & ": " & $response
      outputText[Output] = ""
      output = Debug

  let
    compilationTarget = (
      $kdom.getElementById("compilationtarget").value &
      $kdom.getElementById("compilationmode").value &
      $kdom.getElementById("garbagecollector").value &
      $kdom.getElementById("stylecheck").value
    )
    nimversion = $kdom.getElementById("nimversion").value
    request = %*{"code": $myCodeMirror.getValue(), "compilationTarget": $compilationTarget, "outputFormat": "HTML", "version": nimversion}
  ajaxPost("/compile", @[], $request, cb)

proc shareIx() =
  awaitingShare = true
  proc cb(httpStatus: int, response: cstring) =
    awaitingShare = false
    if httpStatus == 200:
      let ixid = ($response)[13..16]
      outputText[Output] = "https://play.nim-lang.org/#ix=" & ixid
      outputText[Debug] = ""
      output = Output
      if not showingTour:
        loadedIx = ixid
        setHash("#ix=" & loadedIx)
    else:
      outputText[Debug] = "No reply from server:<br/>" & $httpStatus & ": " & $response
      outputText[Output] = ""
      output = Debug

  let request = %*{"code": $myCodeMirror.getValue(), "compilationTarget": "c"}
  ajaxPost("/ix", @[], $request, cb)

proc loadIx(id: string) =
  proc cb(httpStatus: int, response: cstring) =
    if httpStatus == 200:
      outputText[Output] = ""
      outputText[Debug] = ""
      output = Output
      myCodeMirror.setValue(response)
    else:
      outputText[Debug] = "No reply from server:<br/>" & $httpStatus & ": " & $response
      outputText[Output] = ""
      output = Debug

  ajaxGet("/ix/" & id, @[], cb)
  loadedIx = id

proc loadTour(id: string) =
  proc cb(httpStatus: int, response: cstring) =
    if httpStatus == 200:
      outputText[Output] = ""
      outputText[Debug] = ""
      output = Output
      var tourNodes = buildHtml:
        verbatim response.sanitize
      tourContent = tourNodes
    else:
      outputText[Debug] = "No reply from server:<br/>" & $httpStatus & ": " & $response
      outputText[Output] = ""
      output = Debug

  ajaxGet("/tour/" & encodeUriComponent(id), @[], cb)
  loadedTour = id
  showingTour = true

proc postRender(data: RouterData) =
  if knownVersions.len == 1:
    proc cb(httpStatus: int, response: cstring) =
      if httpStatus == 200:
        for version in parseJson($response)["versions"].getElems:
          knownVersions.add version.getStr
    ajaxGet("/versions", @[], cb)
  if myCodeMirror.Element == nil:
    myCodeMirror = newCodeMirror(kdom.getElementById("editor"), js{
      mode: "nim".kstring,
      value: "".kstring,
      tabSize: 2,
      lineNumbers: true,
      theme: "dracula".kstring
    })
    myCodeMirror.setOption("extraKeys", js{
      Tab: proc(cm: CodeMirror) =
        cm.replaceSelection("  ")
      ,
      "Ctrl-Enter": proc(cm: CodeMirror) =
        if (not runningCode): runCode()
      ,
      "Cmd-Enter": proc(cm: CodeMirror) =
        if (not runningCode): runCode()
    })
  if showingTour:
    var tourContent = kdom.getElementById("tour")
    let sections = tourContent.getElementsByTagName("section")
    totalSections = sections.len
    for idx, section in sections:
      if idx != currentSection:
        section.style.display = "none"
      else:
        section.style.display = "block"
        let
          headers = section.getElementsByTagName("h1")
          codes = section.getElementsByTagName("code")
        kdom.getElementById("sectionTitle").innerHtml = $(currentSection + 1) & "/" & $totalSections & ": " & (if headers.len > 0: $headers[0].innerHtml else: "No title")
        if codes.len > 0 and codeCurrentSection != currentSection:
          codeCurrentSection = currentSection
          myCodeMirror.setValue(codes[^1].innerHtml)
          codes[^1].style.display = "none"

proc changeFontSize() =
  let
    editor = kdom.getElementById("editor")
    fontSizeInput = kdom.getElementById("fontsize")
  editor.applyStyle(style(fontSize, fontSizeInput.value & "px"))


proc createDom(data: RouterData): VNode =
  let strhash = $data.hashPart
  if strhash.len > "#ix=".len:
    if strhash[0..3] == "#ix=":
      if loadedIx != strhash[4..^1]:
        loadIx(strhash[4..^1])
  if strhash.len > "#tour=".len:
    if strhash[0..5] == "#tour=":
      if loadedTour != strhash[6..^1]:
        loadTour(strhash[6..^1])
  result = buildHtml(tdiv):
    headerbar:
      nav(class = "navbar"):
        tdiv(class = "container"):
          tdiv(class = "navbar-brand"):
            a(class = "navbar-item", href = "https://play.nim-lang.org", target = "_blank"):
              img(src = "/assets/logo.svg"):
                span: text "Playground"
          tdiv(class = "navbar-menu"):
            tdiv(class = "navbar-start"):
              tdiv(class = "navbar-item has-dropdown is-hoverable"):
                a(class = "navbar-link"):
                  text "Help"
                tdiv(class = "navbar-dropdown"):
                  a(class = "navbar-item navbar-item-dropdown", href = "https://nim-lang.org/install.html", target = "_blank"):
                    text "Download"
                  a(class = "navbar-item navbar-item-dropdown", href = "https://nim-lang.org/learn.html", target = "_blank"):
                    text "Learn"
                  a(class = "navbar-item navbar-item-dropdown", href = "https://nim-lang.org/documentation.html", target = "_blank"):
                    text "Documentation"
                  a(class = "navbar-item navbar-item-dropdown", href = "https://forum.nim-lang.org", target = "_blank"):
                    text "Forum"
                  a(class = "navbar-item navbar-item-dropdown", href = "https://nim-lang.org/community.html", target = "_blank"):
                    text "Chat"
            tdiv(class = "navbar-end"):
              tdiv(class = "navbar-item"):
                a(href = "https://github.com/PMunch/nim-playground-frontend", target = "_blank", title = "Code on GitHub"):
                  span: text "Source"
    mainarea:
      if showingTour:
        baseColumn:
          growContent(id = "tour"):
            tourContent
          bar:
            span(id = "sectionTitle"): text ""
            otherButton(onclick = () => (currentSection = max(currentSection - 1, 0))):
              text "Previous"
            mainButton(onclick = () => (currentSection = min(currentSection + 1, totalSections - 1))):
              text "Next"
      baseColumn:
        bigEditor(id = "editor", class = "monospace"):
          tdiv()
        smallColumn:
          bar:
            if not awaitingShare:
              otherButton(class = "button is-light", onclick = shareIx):
                text "Share link"
            else:
              otherButton(class = "button is-light is-loading"):
                text "Share link"
            if not runningCode:
              mainButton(class = "button is-light", onclick = runCode):
                strong():
                  text "Run!"
                span(class = "buttonhint"):
                  text "(ctrl-enter)"
            else:
              mainButton(class = "button is-light is-loading"):
                text "Run!"
          growContent:
            pre(class = "monospace"):
              verbatim outputText[output]
    footer(class = "footer is-paddingless has-text-centered"):
      details(class = "container is-fullwidth", `open` = "open"):
        summary(class = "is-black"):
          text "Options"
        tdiv(class = "columns"):
          tdiv(class = "column"):
            text " Version:"
            tdiv():
              tdiv(class = "select is-small"):
                select(id = "nimversion", title = "Nim version"):
                  for version in knownVersions:
                    option:
                      text version
          tdiv(class = "column"):
            text "Font size:"
            tdiv(class = "is-small"):
              input(`type` = "number", id = "fontsize", list = "defaultNumbers", value = "13", `min` = "10", `max` = "50", step = "5", required = "required", maxlenght = "2", class = "select is-small", onchange = changeFontSize)
              datalist(id = "defaultNumbers"):
                option(value = "10")
                option(value = "20")
                option(value = "30")
                option(value = "40")
                option(value = "50")
          tdiv(class = "column"):
            a(href = "https://nim-lang.org/docs/backends.html#introduction", target = "_blank", title = "Compiler Target"):
              text " Target:"
            tdiv():
              tdiv(class = "select is-small"):
                select(id = "compilationtarget", title = "Compiler Target", class = "select"):
                  optgroup(label = "Backend"):
                    option(value = $cTarget):
                      text "C"
                    option(value = $cppTarget):
                      text "C++"
                    option(value = $nodejsTarget):
                      text "NodeJS"
                    option(value = $objcTarget):
                      text "ObjectiveC"
                    option(disabled = "disabled"):
                      text "NimScript"
                    option(disabled = "disabled"):
                      text "LLVM IR"
                  optgroup(label = "Frontend"):
                    option(value = $jsTarget):
                      text "JavaScript"
                    option(disabled = "disabled"):
                      text "WebAssembly"
                  optgroup(label = "Diagnostics"):
                    option(value = $checkTarget):
                      text "Check only"
          tdiv(class = "column"):
            a(href = "https://nim-lang.org/docs/nimc.html#additional-compilation-switches", target = "_blank", title = "Optimization level"):
              text " Mode:"
            tdiv():
              tdiv(class = "select is-small"):
                select(id = "compilationmode", title = "Optimization level", class = "select"):
                  option(value = $debugMode):
                    text "Debug"
                  option(value = $releaseMode):
                    text "Release"
                  optgroup(label = "Advanced"):
                    option(value = $dangerReleaseMode):
                      text "Danger"
          tdiv(class = "column"):
            a(href = "https://nim-lang.org/docs/gc.html", target = "_blank", title = "Memory management"):
              text " Garbage collector:"
            tdiv():
              tdiv(class = "select is-small"):
                select(id = "garbagecollector", title = "Memory management", class = "select"):
                  option(value = $defaultGc):
                    text "Default"
                  option(value = $boehmGc):
                    text "Boehm"
                  option(value = $markAndSweepGc):
                    text "Mark&Sweep"
                  option(value = $goGc):
                    text "Go lang"
                  optgroup(label = "Advanced"):
                    option(value = $noneGc):
                      text "None"
                    option(value = $boehmGc):
                      text "Regions"
                    option(value = $destructorsGc):
                      text "Destructor"
          tdiv(class = "column"):
            a(href = "https://nim-lang.org/docs/nep1.html#introduction", target = "_blank", title = "Code style"):
              text " Style:"
            tdiv():
              tdiv(class = "select is-small"):
                select(id = "stylecheck", title = "Code style", class = "select"):
                  option(value = $styleCheckOff):
                    text "Off"
                  option(value = $styleCheckHint):
                    text "Hints"
                  optgroup(label = "Advanced"):
                    option(value = $styleCheckError):
                      text "Errors"
          tdiv(class = "column"):
            text "Show:"
            tdiv():
              tdiv(class = "select is-small"):
                  select(class = "select", onclick = switchOutput):
                    option(value = $styleCheckOff):
                      text "Output"
                    option(value = $styleCheckHint):
                      text "Debug"
        tdiv()


setRenderer createDom, "ROOT", postRender
setForeignNodeId "tour"
setForeignNodeId "editor"
