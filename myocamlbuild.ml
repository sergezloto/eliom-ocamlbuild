(*
 * ocamlbuild API documentation is at http://brion.inria.fr/gallium/index.php/Ocamlbuild
 *)

open Ocamlbuild_plugin

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let path_dependencies_of = Ocamlbuild_pack.Ocaml_utils.path_dependencies_of
let bindir = Ocamlbuild_pack.Ocamlbuild_where.bindir

let split s ch =
  let x = ref [] in
  let rec go s =
    let pos = String.index s ch in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try
    go s
  with Not_found -> !x
                                                                                                                                                                                                                                             
let split_nl s = split s '\n'

let before_space s =
  try
    String.before s (String.index s ' ')
  with Not_found -> s

(* this lists all supported packages *)
let find_packages () =
  List.map before_space (split_nl & run_and_read "ocamlfind list")

(* this is supposed to list available syntaxes, but I dont know how to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* Debug tags for file *)
let print_info f =
  Format.fprintf Format.std_formatter
    "@[<hv 2>Tags for file %s:@ %a@]@." f
    Tags.print (tags_of_pathname f)
let print_paths l =
  Printf.printf "Path length:%d" & List.length l;
  List.iter (fun s -> Printf.printf "%s  " s) l;
  Printf.printf "\n"

(* Prepend commands with bindir *)
let c cmd = A(!bindir / cmd)

(* eliomdep *)
let eliomdep cliserv ?(incs = []) tags arg out = 
  let tags = tags ++ "eliom" ++ "byte" ++ "depends" in
  (* let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in *) 
  (* NOTE: include dirs are used when resolving required modules *)
  let cs = match cliserv with `client -> A"-client" | `server -> A"-server" in
  Cmd(S[c"eliomdep"; cs; A"-modules"; (*is;*) T tags; P arg; Sh ">"; Px out])

(* eliomc infer types *)
let eliomc_i ?(incs = []) tags arg out = 
  let tags = tags ++ "eliom" ++ "byte" ++ "compile" ++ "infer" in
  let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in
  Cmd(S [c"eliomc"; A"-infer"; A"-o"; Px out; is; T tags; P arg])

(* eliomc for server side *)
let eliomc_c ?(incs = []) tags arg out = 
  let tags = tags ++ "eliom" ++ "byte" ++ "compile" in
  let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in
  Cmd(S [c"eliomc"; A"-c"; A"-o"; Px out; is; T tags; P arg])

(* eliomopt for server side *)
let eliomopt_c ?(incs = []) tags arg out = 
  let tags = tags ++ "eliom" ++ "native" ++ "compile" in
  let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in
  Cmd(S [c"eliomopt"; A"-c"; A"-o"; Px out; is; T tags; P arg])
(* eliomopt server side, shared library *)
let eliomopt_shared ?(incs = []) tags arg out = 
  let tags = tags ++ "eliom" ++ "native" ++ "link" in
  let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in
  Cmd(S [c"eliomopt"; A"-shared"; A"-linkall"; A"-o"; Px out; is; T tags; P arg])

(* js_of_ocaml for client side *)
let js_of_ocaml_js  ?(incs = []) tags arg out = 
  let tags = tags ++ "js_of_ocaml" in
  let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in
  Cmd(S [c"js_of_ocaml"; A "-o"; Px out; is; T tags; P arg])

(* js_of_eliom for client side *)
let js_of_eliom_i ?(incs = []) tags arg out =
  let tags = tags ++ "js_of_eliom" ++ "infer" in
  let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in
  Cmd(S[c"js_of_eliom"; A"-c"; A"-o"; Px out; A"-type-dir"; A"_server"; is; T tags; P arg])

(* js_of_eliom for client side *)
let js_of_eliom_c ?(incs = []) tags arg out =
  let tags = tags ++ "js_of_eliom" ++ "compile" in
  let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in
  Cmd(S[c"js_of_eliom"; A"-c"; A"-o"; Px out; A"-type-dir"; A"_server"; is; T tags; P arg])

(* js_of_eliom for client side *)
let js_of_eliom_js ?(incs = []) tags args out =
  let tags = tags ++ "js_of_eliom" ++ "jslink" in
  let is = S (List.map (fun s -> S [A"-I"; P s]) incs) in
  let p_args = S (List.map (fun a -> P a) args) in
  Cmd(S[c"js_of_eliom"; A"-linkall"; A"-o"; Px out; is; T tags; p_args])

(* Prepares an eliom compilation; add build targets as per dependancies; eliom is an eliom or eliomi file *)
let prepare_eliom_compile =
  let level = ref 0 in
  fun build eliom ->
    incr level;
    Ocamlbuild_pack.Log.dprintf 3 "%*sPrepare dependencies for %s" !level "-" eliom;
    let this_module = module_name_of_pathname eliom in
    let dir = Pathname.dirname eliom in
    let include_dirs = Pathname.include_dirs_of dir in
    let _ = List.iter (fun p -> Ocamlbuild_pack.Log.dprintf 5 "Path for %s = %s" eliom p) include_dirs in
    let modules = path_dependencies_of eliom in
    let modules = List.map (fun (x,m) -> (x, module_name_of_pathname m)) modules in
    let modules = List.filter (fun (_,m) -> m <> this_module) modules in
    let _ = List.iter (fun (_, m) -> Ocamlbuild_pack.Log.dprintf 5 "%s depends on %s" eliom m) modules in
    let results =
      let modfile m =  
        let exts = ["cmi"] in
        let mf = expand_module include_dirs m exts in
        List.iter (Ocamlbuild_pack.Log.dprintf 5 "%*smodule %s in file %s" !level "-" m) mf;
        mf
      in
      build (List.map (fun (_, m) -> modfile m) modules) in
    List.iter2 begin fun (mandatory, name) res ->
      match mandatory, res with
      | _, Outcome.Good _ -> ()
      | `mandatory, Outcome.Bad exn ->
          if !Options.ignore_auto then
            Ocamlbuild_pack.Log.dprintf 3 "Warning: Failed to build the mandatory module \
                       %s requested by ocamldep" name
          else raise exn
      | `just_try, Outcome.Bad _ -> 
          Ocamlbuild_pack.Log.dprintf 5 "Warning: Failed to build the optional module %s requested by ocamldep" name
    end modules results;
    decr level

let add_rules () =
  let eliom_srv_includes =
    let incs = !Options.include_dirs in
    (* Server compilation: add include paths prepended with _server/ *)
    incs @ (List.map (fun p -> "_server/" ^ p) incs) in
  let eliom_cli_includes =
    let incs = !Options.include_dirs in
    (* Client compilation: add include paths prepended with _client/ *)
    incs @ (List.map (fun p -> "_client/" ^ p) incs) in
  begin
    copy_rule
      "copy: mllib -> _server/mllib"
      "%.mllib" "_server/%.mllib";
    rule
      "eliomdep: eliomi -> _server/eliomi.depends"
      ~prod:"_server/%.eliomi.depends"
      ~dep:"%.eliomi"
      begin
        fun env _build -> 
          begin
            let src = env "%.eliomi"
            and dst = env "_server/%.eliomi.depends" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomdep `server ~incs tags src dst
          end
      end;
    rule
      "eliomdep: eliom -> _server/eliom.depends"
      ~prod:"_server/%.eliom.depends"
      ~dep:"%.eliom"
      begin
        fun env _build -> 
          begin
            let src = env "%.eliom"
            and dst = env "_server/%.eliom.depends" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomdep `server ~incs tags src dst
          end
      end;
    rule
      "eliomc: eliomi -> _server/type_mli"
      ~prod:"_server/%.type_mli"
      ~deps:["%.eliomi"; "_server/%.eliomi.depends"]
      begin
        fun env _build -> 
          begin
            let _ =
              let depfile = env "_server/%.eliomi" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliomi"
            and dst = env "_server/%.type_mli" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomc_i ~incs tags src dst
          end
       end; 
    rule
      "eliomc: eliom -> _server/type_mli"
      ~prod:"_server/%.type_mli"
      ~deps:["%.eliom"; "_server/%.eliom.depends"]
      begin
        fun env _build -> 
          begin
            let _ =
              let depfile = env "_server/%.eliom" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliom"
            and dst = env "_server/%.type_mli" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomc_i ~incs tags src dst
          end
       end; 
    rule
      "eliomc: eliomi -> _server/cmi"
      ~prod:"_server/%.cmi"
      ~deps:["%.eliomi"; "_server/%.type_mli"]
      begin
        fun env _build -> 
          begin
            let _ =
              let depfile = env "_server/%.eliomi" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliomi"
            and dst = env "_server/%.cmi" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomc_c ~incs tags src dst
          end
       end; 
    rule
      "eliomc: _server/cmi & eliom -> _server/cmo"
      ~prod:"_server/%.cmo"
      ~deps:["%.eliomi"; "_server/%.cmi"; "%.eliom"] (* .eliomi not present => this rule is skipped *)
      begin
        fun env _build -> 
          begin
            let _ =
              let depfile = env "_server/%.eliom" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliom"
            and dst = env "_server/%.cmo" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomc_c ~incs tags src dst
          end
       end; 
    rule
      "eliomc: eliom -> _server/cmo & _server/cmi"
      ~prods:["_server/%.cmo"; "_server/%.cmi"]
      ~deps:["%.eliom"; "_server/%.type_mli"]
      begin
        fun env _build -> 
          begin
            let _ =
              let depfile = env "_server/%.eliom" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliom"
            and dst = env "_server/%.cmo" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomc_c ~incs tags src dst
          end
      end;
    rule
      "eliomopt: _server/cmi & eliom -> _server/cmx"
      ~prod:"_server/%.cmx"
      ~deps:["%.eliomi";"_server/%.cmi"; "%.eliom"] (* .eliomi not present => this rule is skipped *)
      begin
        fun env _build -> 
          begin
            let _ =
              let depfile = env "_server/%.eliom" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliom"
            and dst = env "_server/%.cmx" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomopt_c ~incs tags src dst
          end
       end; 
    rule
      "eliomopt: eliom -> _server/cmx & _server/o"
      ~prods:["_server/%.cmx"; "_server/%.cmi"; "_server/%.o"]
      ~deps:["%.eliom";"_server/%.type_mli"]
      begin
        fun env _build -> 
          begin
            let _ =
              let depfile = env "_server/%.eliom" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliom"
            and dst = env "_server/%.cmx" in
            let tags = tags_of_pathname src in
            let incs = eliom_srv_includes in
            eliomopt_c ~incs tags src dst
          end
      end;
    copy_rule
      "copy: mllib -> _client/mllib"
      "%.mllib" "_client/%.mllib";
    rule
      "eliomdep: eliom -> _client/eliom.depends"
      ~prod:"_client/%.eliom.depends"
      ~dep:"%.eliom"
      begin
        fun env _build -> 
          begin
            let src = env "%.eliom"
            and dst = env "_client/%.eliom.depends" in
            let tags = tags_of_pathname src in
            let incs = !Options.include_dirs in
            (*
            print_info src;
            print_paths incs;
            *)
            eliomdep `client ~incs tags src dst
          end
      end;
    rule
      "js_of_eliom: eliomi -> _client/cmi"
      ~prods:["_client/%.cmi"]
      ~deps:["%.eliomi";"_client/%.eliomi.depends";"_server/%.type_mli"]
      begin
        fun env _build ->
            let _ =
              let depfile = env "_client/%.eliomi" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliomi"
            and dst = env "_client/%.cmi" in
            let tags = tags_of_pathname src in
            let incs = eliom_cli_includes in
            js_of_eliom_c ~incs tags src dst
      end;
    rule
      "js_of_eliom: eliom -> _client/cmo"
      ~prods:["_client/%.cmo";"_client/%.cmi"]
      ~deps:["%.eliom";"_client/%.eliom.depends";"_server/%.type_mli"]
      begin
        fun env _build ->
            let _ =
              let depfile = env "_client/%.eliom" in
              prepare_eliom_compile _build depfile
            in
            let src = env "%.eliom"
            and dst = env "_client/%.cmo" in
            let tags = tags_of_pathname src in
            let incs = eliom_cli_includes in
            js_of_eliom_c ~incs tags src dst
      end;
    rule 
      "js_of_eliom: _client/cma -> _client/js"
      ~prod:"_client/%.js"
      ~dep:"_client/%.cma"
      begin
        fun env _build ->
            let src = env "_client/%.cma"
            and dst = env "_client/%.js" in            
            (* Proceed with linking *)
            let tags = tags_of_pathname src in
            let incs = !Options.include_dirs in
            js_of_eliom_js ~incs tags [src] dst
      end;
    rule 
      "js_of_ocaml: byte -> js"
      ~prod:"%.js"
      ~dep:"%.byte"
      begin
        fun env _build ->
            let src = env "%.byte"
            and dst = env "%.js" in            
            (* Proceed with linking *)
            let tags = tags_of_pathname src in
            let incs = !Options.include_dirs in
            js_of_ocaml_js ~incs tags src dst
      end;
  end

let _ = 
  dispatch begin function

  | Before_options ->
      Options.make_links := true;

  | After_options ->
      ()
  | Before_rules ->
      add_rules ();
      ()

  | After_rules ->
      (* Add some tags to the standard set *)
      flag ["ocaml"; "compile"; "verbose"] & A"-verbose";
      flag ["ocaml"; "compile"; "unsafe"] & A"-unsafe";
      flag ["ocaml"; "compile"; "noassert"] & A"-noassert";
      flag ["ocaml"; "compile"; "principal"] & A"-principal";

      flag ["eliom"; "compile"; "debug"] & S[A"-g"];
      flag ["eliom"; "compile"; "unsafe"] & A"-unsafe";
      flag ["eliom"; "compile"; "verbose"] & A"-verbose";
      flag ["eliom"; "compile"; "noassert"] & A"-noassert";
      flag ["eliom"; "compile"; "annot"] & A"-annot";
      flag ["eliom"; "compile"; "principal"] & A"-principal";
      
      flag ["js_of_ocaml"; "debug"] & S[A"-pretty";A"-debuginfo";A"-noinline"];
      flag ["js_of_eliom"; "debug"] & S[A"-g";A"-jsopt";A"-pretty";A"-jsopt";A"-debuginfo";A"-jsopt";A"-noinline"];
      (* For each ocamlfind package, inject the -package option
         when compiling, computing dependencies, generating
         documentation and linking. *)
      List.iter begin fun pkg ->
        flag ["eliom"; "compile"; "package("^pkg^")"] & S[A"-package"; A pkg];
        flag ["eliom"; "compile"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["js_of_eliom"; "compile"; "cpackage("^pkg^")"] & S[A"-package"; A pkg];
        flag ["js_of_eliom"; "compile"; "cpkg_"^pkg] & S[A"-package"; A pkg];
        flag ["js_of_eliom"; "jslink"; "cpackage("^pkg^")"] & S[A"-package"; A pkg];
        flag ["js_of_eliom"; "jslink"; "cpkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
      end & find_packages ();
      (* Like -package but for extensions syntax. Morover -syntax is useless
      * when linking. *)
      List.iter begin fun syntax ->
        flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
      end (find_syntaxes ());
     
      (* SZ: Support for -linkpkg. Add link _tags *)
      flag ["ocaml"; "link"; "linkpkg"] & A"-linkpkg";

      (* The default "thread" tag is not compatible with ocamlfind.
         Indeed, the default rules add the "threads.cma" or
         "threads.cmxa" options when using this tag. When using the
         "-linkpkg" option with ocamlfind, this module will then be
         added twice on the command line.

         To solve this, one approach is to add the "-thread" option
         when using the "threads" package using the previous
         plugin. *)
       flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])

  | Before_hygiene
  | After_hygiene -> 
    ()
end
