
module internal FInvoke.JsProxy

open System
open System.Reflection
open Futility

let private top = "
  (function () {
    var top = this;
    if (top['Pound'] === undefined) {
      Pound = { top: this, host: {} }
      Pound.host.html = top['window'] === top;
      Pound.host.mscom = top['ActiveXObject'] !== undefined;
      Pound.request = function (url, method, data, callback, error, async) {
        if (async === undefined || async === null) {
          async = true;
        }
        var req = Pound.host.mscom ? new ActiveXObject ('Microsoft.XMLHTTP') : new XMLHttpRequest ();
        req.open (method, url, async);
        var done = function () {
          if ((req.status >= 200 && req.status <= 299) || req.status === 304) {
            if (callback !== undefined && callback !== null) {
              callback (req.responseText, req);
            }
          }
          else {
            if (error !== undefined && error !== null) {
              error (req.responseText, req);
            }
            else {
              throw req.responseText || 'Unknown error sending request.';
            }
          }
        };
        if (async) {
          req.onreadystatechange = function () {
            if (req.readyState === 4) {
              done ();
            }
          };
        }
        data = data !== undefined && data !== null ? data : null;
        req.send (data);
        if (!async) {
          done ();
        }
      };
    }
  }) ();
"
let private tmProxy = "
  (function () {
    var aid = 0;
    var id = function () { return aid++; };
    var defUrl = '#defurl#';
    #namespaces#
    Pound.top.#name# = function (url) {
      return {
        #stubs#
      };
    };
  }) ();
"
let private tmTopNamespace = "
  if (Pound.top['#namespace#'] === undefined) {
    Pound.top['#namespace#'] = {};
  }
"
let private tmNamespace = "
  if (Pound.top.#parent#['#namespace#'] === undefined) {
    Pound.top.#parent#['#namespace#'] = {};
  }
"
let private tmStub = "
  '#methodname#': function (params, callback, error) {
    var body = JSON.stringify ({
      jsonrpc: '2.0',
      method: '#name#',
      params: params || [],
      id: id ()
    });
    Pound.request (url || defUrl, 'POST', body, function (res) {
      res = JSON.parse (res);
      if (res.result !== undefined) {
        if (callback !== undefined)
          callback (res.result);
      }
      else if (res.error !== undefined) {
        if (error !== undefined) {
          error (res.error);
        }
        else {
          throw res.error.message;
        }
      }
    }, error, #async#);
  }
"

let create url sv =
  let url = "/" + url
  let st = sv.GetType ()
  let stns =
    let fns = st.FullName |> String.replace "+" "."
    fns |> String.left (fns.LastIndexOf ".")
  let stname = st.FullName |> String.replace "+" "."
  let namespaces =
    let topns = stns |> String.left (stns.IndexOfAny [| '.'; '+' |])
    let top =
      tmTopNamespace
      |> String.replace "#namespace#" topns
    let fold (parent, script) ns =
      parent + "." + ns
      , script +
        (tmNamespace
          |> String.replace "#parent#" parent
          |> String.replace "#namespace#" ns
        )
    stns
    |> String.split ['.'; '+']
    |> List.tail
    |> List.fold fold (topns, top)
    |> snd
  let stubs =
    let replace (b : bool) (suffix) (p : PropertyInfo) =
      tmStub
      |> String.replace "#methodname#" (p.Name + suffix)
      |> String.replace "#name#" p.Name
      |> String.replace "#async#" (string b |> String.lower)
    let cstubs (b : bool) suffix =
      st
      |> FSharpRecord.getFields
      |> List.map (replace b suffix)
    (cstubs true "") @ (cstubs false "Sync")
    |> String.concat ","
  top + tmProxy
  |> String.replace "#defurl#" url
  |> String.replace "#namespaces#" namespaces
  |> String.replace "#name#" stname
  |> String.replace "#stubs#" stubs
