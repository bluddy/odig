(*---------------------------------------------------------------------------
   Copyright (c) 2018 The odig programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Odig_support
open B00_std
open B00

let odig_version = "%%VERSION%%"

let link_if_exists src dst = match src with
| None -> ()
| Some src ->
    Os.Path.symlink ~force:true ~make_path:true ~src dst |> Log.if_error ~use:()

(* Theme handling *)

let ocaml_manual_pkg = "ocaml-manual"

let get_theme conf =
  let ts = B00_odoc.Theme.of_dir (Conf.share_dir conf) in
  let odig_theme =
    let odig t = B00_odoc.Theme.name t = B00_odoc.Theme.odig_default in
    match List.find odig ts with exception Not_found -> None | t -> Some t
  in
  let name = Conf.odoc_theme conf in
  let fallback = match odig_theme with
  | Some t -> Some (B00_odoc.Theme.name t)
  | None -> Some (B00_odoc.Theme.odoc_default)
  in
  Log.if_error ~level:Log.Warning ~use:odig_theme @@
  Result.bind (B00_odoc.Theme.find ~fallback name ts) @@ fun t ->
  Ok (Some t)

let write_ocaml_manual_theme conf m theme =
  let write_original_css conf ~o =
    let css = Fpath.(Conf.doc_dir conf / ocaml_manual_pkg / "manual.css") in
    B00.Memo.delete m o @@ fun () ->
    B00.Memo.file_ready m css;
    Memo.copy m ~src:css o
  in
  let manual_dir = Fpath.(Conf.html_dir conf / ocaml_manual_pkg) in
  match Os.Dir.exists manual_dir |> Log.if_error ~use:false with
  | false -> ()
  | true ->
      let manual_css = Fpath.(manual_dir / "manual.css") in
      let theme_manual_css = match theme with
      | None -> None
      | Some t ->
          let css = Fpath.(B00_odoc.Theme.path t / "manual.css") in
          if Os.File.exists css |> Log.if_error ~use:false
          then Some (t, css)
          else None
      in
      match theme_manual_css with
      | None -> write_original_css conf ~o:manual_css
      | Some (t, css) ->
          (* We copy the theme again in ocaml-manual because of FF. *)
          let to_dir = Fpath.(manual_dir / B00_odoc.Theme.default_uri) in
          B00.Memo.delete m to_dir @@ fun () ->
          B00_odoc.Theme.write m t ~to_dir;
          Memo.write m manual_css @@ fun () ->
          Ok "@charset UTF-8;\n@import url(\"_odoc-theme/manual.css\");"

let write_theme conf m theme =
  let to_dir = Fpath.(Conf.html_dir conf / B00_odoc.Theme.default_uri) in
  B00.Memo.delete m to_dir @@ fun () ->
  (match theme with None -> () | Some t -> B00_odoc.Theme.write m t ~to_dir);
  write_ocaml_manual_theme conf m theme

(* Builder *)

type resolver =
  { mutable cobjs_by_digest : Doc_cobj.t list Digest.Map.t;
    mutable cobj_deps : (B00_odoc.Compile.Dep.t list Memo.Fut.t) Fpath.Map.t;
    mutable pkgs_todo : Pkg.Set.t;
    mutable pkgs_seen : Pkg.Set.t; }

type builder =
  { m : Memo.t;
    conf : Conf.t;
    odoc_dir : Fpath.t;
    html_dir : Fpath.t;
    theme : B00_odoc.Theme.t option;
    index_title : string option;
    index_intro : Fpath.t option;
    pkg_deps : bool;
    tag_index : bool;
    cobjs_by_modname : Doc_cobj.t list String.Map.t;
    r : resolver; }

let builder m conf ~index_title ~index_intro ~pkg_deps ~tag_index pkgs_todo =
  let cache_dir = Conf.cache_dir conf in
  let odoc_dir = Fpath.(cache_dir / "odoc") in
  let html_dir = Conf.html_dir conf in
  let theme = get_theme conf in
  let cobjs_by_modname =
    let add p i acc = Doc_cobj.by_modname ~init:acc (Pkg_info.doc_cobjs i) in
    Pkg.Map.fold add (Conf.pkg_infos conf) String.Map.empty
  in
  let cobjs_by_digest = Digest.Map.empty in
  let cobj_deps = Fpath.Map.empty in
  let pkgs_todo = Pkg.Set.of_list pkgs_todo in
  let pkgs_seen = Pkg.Set.empty in
  { m; conf; odoc_dir; html_dir; theme; index_title; index_intro; pkg_deps;
    tag_index; cobjs_by_modname;
    r = { cobjs_by_digest; cobj_deps; pkgs_todo; pkgs_seen } }

let require_pkg b pkg =
  if Pkg.Set.mem pkg b.r.pkgs_seen || Pkg.Set.mem pkg b.r.pkgs_todo then () else
  (Log.debug (fun m -> m "Package request %a" Pkg.pp pkg);
   b.r.pkgs_todo <- Pkg.Set.add pkg b.r.pkgs_todo)

let pkg_assets_dir = "_assets"
let pkg_html_dir b pkg = Fpath.(b.html_dir / Pkg.name pkg)
let pkg_odoc_dir b pkg = Fpath.(b.odoc_dir / Pkg.name pkg)

let odoc_file_for_cobj b cobj =
  let pkg = Doc_cobj.pkg cobj in
  let root = Pkg.path pkg in
  let dst = pkg_odoc_dir b pkg in
  let cobj = Doc_cobj.path cobj in
  Fpath.(reroot ~root ~dst cobj -+ ".odoc")

let odoc_file_for_mld b pkg mld = (* assume mld names are flat *)
  let page = Fmt.str "page-%s" (Fpath.basename mld) in
  Fpath.(pkg_odoc_dir b pkg / page -+ ".odoc")

let require_cobj_deps b cobj = (* Also used to find the digest of cobj *)
  let add_cobj_by_digest b cobj d =
    let cobjs = try Digest.Map.find d b.r.cobjs_by_digest with
    | Not_found -> []
    in
    b.r.cobjs_by_digest <- Digest.Map.add d (cobj :: cobjs) b.r.cobjs_by_digest
  in
  let set_cobj_deps b cobj dep =
    b.r.cobj_deps <- Fpath.Map.add (Doc_cobj.path cobj) dep b.r.cobj_deps
  in
  match Fpath.Map.find (Doc_cobj.path cobj) b.r.cobj_deps with
  | deps -> deps
  | exception Not_found ->
      let m = Memo.with_mark b.m (Pkg.name (Doc_cobj.pkg cobj)) in
      let fut_deps, set_deps = Memo.Fut.create m in
      let odoc_file = odoc_file_for_cobj b cobj in
      let deps_file = Fpath.(odoc_file + ".deps") in
      set_cobj_deps b cobj fut_deps;
      begin
        Memo.file_ready m (Doc_cobj.path cobj);
        B00_odoc.Compile.Dep.write m (Doc_cobj.path cobj) ~o:deps_file;
        B00_odoc.Compile.Dep.read m deps_file @@ fun deps ->
        let rec loop acc = function
        | [] -> set_deps (Some acc)
        | d :: ds ->
            match B00_odoc.Compile.Dep.name d = Doc_cobj.modname cobj with
            | true ->
                add_cobj_by_digest b cobj (B00_odoc.Compile.Dep.digest d);
                loop acc ds
            | false ->
                loop (d :: acc) ds
        in
        loop [] deps
      end
      fut_deps

let cobj_deps b cobj k = Memo.Fut.await (require_cobj_deps b cobj) k
let cobj_deps_to_odoc_deps b deps k ~esy_deps =
  (* For each dependency this tries to find a cmi, cmti or cmt file
     that matches the dependency name and digest. We first look by
     dependency name in the universe and then request on the fly the
     computation of their digest via [require_cobj_deps] which updates
     b.cobjs_by_digest as a side effect. Once the proper compilation
     object has been found we then return the odoc file for that
     file. Since we need to make sure that this odoc file actually
     gets built its package is added to the set of packages that need
     to be built; unless [b.pkg_deps] is false. *)
  let candidate_cobjs dep =
    let n = B00_odoc.Compile.Dep.name dep in
    let cobjs = match String.Map.find n b.cobjs_by_modname with
    | cobjs -> cobjs
    | exception Not_found ->
        Log.debug (fun m -> m "Cannot find compilation object for %s" n);
        []
    in
    dep, List.map (fun cobj -> cobj, (require_cobj_deps b cobj)) cobjs
  in
  let resolve_dep (dep, candidates) acc k =
    let rec loop = function
    | [] ->
        Log.debug begin fun m ->
          m "Cannot resolve dependency for %a" B00_odoc.Compile.Dep.pp dep
        end;
        k acc
    | (cobj, deps) :: cs ->
        Memo.Fut.await deps begin fun _ ->
          let digest = B00_odoc.Compile.Dep.digest dep in
          match Digest.Map.find digest b.r.cobjs_by_digest with
          | exception Not_found -> loop cs
          | cobj :: _ (* FIXME Log on debug. *) ->
              let handle_pkg pkg =
                if b.pkg_deps then begin
                  require_pkg b pkg;
                  k (odoc_file_for_cobj b cobj :: acc)
              | false ->
                  let pkg = Doc_cobj.pkg cobj in
                  if Pkg.Set.mem pkg b.r.pkgs_todo ||
                     Pkg.Set.mem pkg b.r.pkgs_seen
                  then k (odoc_file_for_cobj b cobj :: acc)
                  else loop cs
                end
              in
              let pkg = Doc_cobj.pkg cobj in
              begin match esy_deps with
              | None -> handle_pkg pkg
              | Some esy_deps ->
                  let name = String.lowercase @@ Pkg.out_dirname ~subver:true pkg in
                  (*String.Set.iter (fun s -> print_endline s) esy_deps; *)
                  (* Printf.printf "name = %s, found = %B\n" name
                    (name = "ocaml" || String.Set.mem name esy_deps); *) (* debug *)
                  if name = "ocaml" ||
                     String.Set.mem name esy_deps then handle_pkg pkg
                  else loop cs
              end
          | [] -> assert false
        end
    in
    loop candidates
  in
  let dep_candidates_list = List.map candidate_cobjs deps in
  let rec loop cs acc = match cs with
  | [] -> k acc
  | c :: cs -> resolve_dep c acc (loop cs)
  in
  loop dep_candidates_list []

let exclusions = ["dune"; "ocmalbuild"; "ocamlfind"]

(* esy lists all of its dependencies in .info files *)
let esy_pkg_deps pkg =
  let name = Pkg.name pkg in
  if name = "ocaml" then String.Set.empty
  else match Pkg.version pkg with
  | None -> invalid_arg name
  | Some (ver, subver) ->
    (* Get the info file *)
    let path = Fpath.(parent @@ parent @@ parent @@ parent @@ Pkg.path pkg) in
    let info_name = Esy.long_name_of_pkg name ver subver ^ ".info" in
    let file = Fpath.(path / "b" / info_name) in
    let dep_list =
      match Yojson.Basic.from_file @@ Fpath.to_string file with
      | `Assoc l ->
          begin
            match List.assoc "idInfo" l with
              | `Assoc l ->
                  begin
                    match List.assoc "dependencies" l with
                    | `List l ->
                          List.map (function
                          | `String s -> s
                          | _ -> invalid_arg "Expected json string") l
                    | exception Not_found -> invalid_arg "Couldn't find dependencies"
                    | _ -> invalid_arg "Expected json list"
                  end
              | exception Not_found -> invalid_arg "Couldn't find 'idInfo'"
              | _ -> invalid_arg "Expected json assoc"
          end
      | exception _ -> []
      | _ -> invalid_arg "Expected json assoc"
    in
    let dep_list =
      (* get only opam__ dependencies *)
      List.filter (fun s ->
        try
          let prefix = String.sub s 0 4 in
          prefix = "opam" || prefix = "ocaml"
        with Invalid_argument _ -> false) dep_list
    in
    let dep_list =
    List.filter_map (fun s ->
        let name, ver, subver = Esy.name_ver_of_long_name s in
        if List.mem name exclusions then None
        else
           Some(Printf.sprintf "%s.%s-%s" name ver subver))
      dep_list
    in
    String.Set.of_list dep_list

let esy_dep_map b =
  (* add dependencies of all packages via esy to a map *)
  let global_dep_map =
    Pkg.Set.fold (fun pkg map ->
        String.Map.add (Pkg.out_dirname pkg) (esy_pkg_deps pkg) map)
      b.pkgs_todo
      String.Map.empty
  in
  (* debug *)
  (*
  print_endline "--- Global Dep Map: ---";
  String.Map.iter (fun k v ->
    print_endline @@ k^":";
    String.Set.iter (fun p -> print_string @@ p^", ") v;
    print_newline ()
    )
  global_dep_map;
  print_endline "--- End Global Dep Map: ---";
  *)

  let pkg_set = String.Map.fold
    (fun pkg _ set -> String.Set.add pkg set)
    global_dep_map String.Set.empty
  in
  (* add transitive dependencies to new map *)
  (* @sibling_deps: builds up a set of deps across sibling pkgs
  *  @done_map: the map we're building up of pkg->dep set
  *  @pass_siblings: don't build up siblings at highest level, but do
  *    at lower levels.
  *)
  let rec add_deps ?(pass_siblings=true) pkg ((done_map, sibling_deps) as acc) =
    (* First see if this pkg was already handled *)
    match String.Map.find_opt pkg done_map with
    | Some s ->
        let done_set =
          String.Set.union sibling_deps s |>
          String.Set.add pkg in
        (done_map, done_set)
    | None ->
      (* Find the direct deps of this pkg *)
      let direct_deps =
        try
          String.Map.find pkg global_dep_map
        with Not_found ->
          print_endline @@ "Warning: esy_map build: couldn't find "^pkg;
          String.Set.empty
      in
      (* Now iterate over the dependencies *)
      let done_map', child_deps =
        String.Set.fold add_deps direct_deps (done_map, String.Set.empty)
      in
      let my_map = String.Map.add pkg child_deps done_map' in
      let pass_deps =
        if pass_siblings then
          String.Set.union sibling_deps child_deps |>
          String.Set.add pkg
        else
          String.Set.empty
      in
      my_map, pass_deps
  in
  let esy_map, _ =
    String.Set.fold (add_deps ~pass_siblings:false) pkg_set
      (String.Map.empty, String.Set.empty)
  in
  esy_map

let cobj_to_odoc b cobj ~esy_map =
  let odoc = odoc_file_for_cobj b cobj in
  begin
    cobj_deps b cobj @@ fun deps ->
    let pkg = Doc_cobj.pkg cobj in
    let pkg_name = String.lowercase_ascii @@ Pkg.out_dirname ~subver:true pkg in
    let esy_deps =
      match esy_map with
      | Some esy_map ->
        (* debug *)
        (* print_endline @@ "pkg_name is "^pkg_name; *)
        if pkg_name = "ocaml" then None
        else String.Map.find_opt pkg_name esy_map
      | None -> None
    in
    cobj_deps_to_odoc_deps b deps @@ fun odoc_deps ->
    let pkg = Pkg.out_name (Doc_cobj.pkg cobj) in
    let hidden = Doc_cobj.hidden cobj in
    let cobj = Doc_cobj.path cobj in
    B00_odoc.Compile.to_odoc b.m ~hidden ~pkg ~odoc_deps cobj ~o:odoc
  end;
  odoc



let mld_to_odoc b pkg pkg_odocs mld =
  let odoc = odoc_file_for_mld b pkg mld in
  let pkg = Pkg.out_dirname pkg in
  let odoc_deps =
    (* XXX odoc compile-deps does not work on .mld files, so we
       simply depend on all of the package's odoc files. This is
       needed for example for {!modules } to work in the index.
       trefis says: In the long term this will be solved since all
       reference resolution will happen at the `html-deps` step. For
       now that seems a good approximation. *)
    pkg_odocs
  in
  B00_odoc.Compile.to_odoc b.m ~pkg ~odoc_deps mld ~o:odoc;
  odoc

let index_mld_for_pkg b pkg pkg_info _pkg_odocs ~user_index_mld =
  let index_mld = Fpath.(pkg_odoc_dir b pkg / "index.mld") in
  let write_index_mld ~user_index =
    let reads = Option.to_list user_index_mld in
    let reads = match Opam.file pkg with
    | None -> reads
    | Some file -> Memo.file_ready b.m file; file :: reads
    in
    let stamp =
      (* Influences the index content; we could relativize file paths *)
      let fields = List.rev_map snd Pkg_info.field_names in
      let data = List.rev_map (fun f -> Pkg_info.get f pkg_info) fields in
      let data = odig_version :: (Pkg.name pkg) :: List.concat data in
      Hash.to_bytes (Memo.hash_string b.m (String.concat "" data))
    in
    Memo.write b.m ~stamp ~reads index_mld @@ fun () ->
    let with_tag_links = b.tag_index in
    Ok (Odig_odoc_page.index_mld b.conf pkg pkg_info ~with_tag_links
          ~user_index)
  in
  begin match user_index_mld with
  | None -> write_index_mld ~user_index:None
  | Some i -> Memo.read b.m i @@ fun s -> write_index_mld ~user_index:(Some s)
  end;
  index_mld

let mlds_to_odoc b pkg pkg_info pkg_odocs mlds =
  let rec loop ~user_index_mld pkg_odocs = function
  | mld :: mlds ->
      Memo.file_ready b.m mld;
      let odocs, user_index_mld = match Fpath.basename mld = "index.mld" with
      | false -> mld_to_odoc b pkg pkg_odocs mld :: pkg_odocs, user_index_mld
      | true -> pkg_odocs, Some mld
      in
      loop ~user_index_mld odocs mlds
  | [] ->
      (* We do the index at the end due to a lack of functioning
         of `compile-deps` on mld files this increases the chances
         we get the correct links towards other mld files since those
         will be in [odocs]. *)
      let mld = index_mld_for_pkg b pkg pkg_info pkg_odocs ~user_index_mld in
      (mld_to_odoc b pkg pkg_odocs mld :: pkg_odocs)
  in
  loop ~user_index_mld:None [] mlds

let html_deps_resolve b deps ~esy_deps k =
  let deps = List.rev_map B00_odoc.Html.Dep.to_compile_dep deps in
  cobj_deps_to_odoc_deps b deps k ~esy_deps

let link_odoc_assets b pkg pkg_info =
  let src = Doc_dir.odoc_assets_dir (Pkg_info.doc_dir pkg_info) in
  let dst = Fpath.(pkg_html_dir b pkg / pkg_assets_dir) in
  link_if_exists src dst

let link_odoc_doc_dir b pkg pkg_info =
  let src = Doc_dir.dir (Pkg_info.doc_dir pkg_info) in
  let dst = Fpath.(pkg_html_dir b pkg / "_doc-dir") in
  link_if_exists src dst

let pkg_to_html b pkg ~esy_map =
  let b = { b with m = Memo.with_mark b.m (Pkg.name pkg) } in
  let pkg_info = try Pkg.Map.find pkg (Conf.pkg_infos b.conf) with
  | Not_found -> assert false
  in
  let pkg_name = String.lowercase_ascii @@ Pkg.out_dirname ~subver:true pkg
  in
  let cobjs = Pkg_info.doc_cobjs pkg_info in
<<<<<<< HEAD
  let mlds = Docdir.odoc_pages (Pkg_info.docdir pkg_info) in
  if cobjs = [] && mlds = [] then () else
  let pkg_html_dir = pkg_html_dir b pkg in
  let pkg_odoc_dir = pkg_odoc_dir b pkg in
  Memo.delete b.m pkg_html_dir @@ fun () ->
  Memo.delete b.m pkg_odoc_dir @@ fun () ->
  let odocs = List.map (cobj_to_odoc b ~esy_map) cobjs in
  let mld_odocs = mlds_to_odoc b pkg pkg_info odocs mlds in
  let odoc_files = List.rev_append odocs mld_odocs in
  let deps_file = Fpath.(pkg_odoc_dir / Pkg.name pkg + ".html.deps") in
  B00_odoc.Html.Dep.write b.m ~odoc_files pkg_odoc_dir ~o:deps_file;
  B00_odoc.Html.Dep.read b.m deps_file @@ fun deps ->
  let esy_deps =
    match esy_map with
    | Some esy_map ->
      if pkg_name = "ocaml" then None
      else String.Map.find_opt pkg_name esy_map
    | None -> None
  in
  html_deps_resolve b deps @@ fun odoc_deps_res ->
  (* XXX html deps is a bit broken make sure we have at least our
     own files as deps maybe related to compiler-deps not working on .mld
     files *)
  let odoc_deps = Fpath.uniquify (List.rev_append odoc_files odoc_deps_res) in
  let theme_uri = match b.theme with
  | Some _ -> Some B00_odoc.Theme.default_uri | None -> None
  in
  let html_dir = b.html_dir in
  let to_html = B00_odoc.Html.write b.m ?theme_uri ~html_dir ~odoc_deps in
  List.iter to_html odoc_files;
  link_odoc_assets b pkg pkg_info;
  link_odoc_doc_dir b pkg pkg_info

let index_intro_to_html b k = match b.index_intro with
| None -> k None
| Some mld ->
    let is_odoc _ _ f acc = if Fpath.has_ext ".odoc" f then f :: acc else acc in
    let odoc_deps = Os.Dir.fold_files ~recurse:true is_odoc b.odoc_dir [] in
    let odoc_deps = Memo.fail_if_error b.m odoc_deps in
    let o = Fpath.(b.odoc_dir / "index-header.html") in
    Memo.file_ready b.m mld;
    B00_odoc.Html_fragment.cmd b.m ~odoc_deps mld ~o;
    Memo.read b.m o @@ fun index_header -> k (Some index_header)

let write_pkgs_index b ~ocaml_manual_uri =
  let add_pkg_data pkg_infos acc p = match Pkg.Map.find p pkg_infos with
  | exception Not_found -> acc
  | info ->
      let version = Pkg_info.get `Version info in
      let synopsis = Pkg_info.get `Synopsis info in
      let tags = Pkg_info.get `Tags info in
      let ( ++ ) = List.rev_append in
      version ++ synopsis ++ tags ++ acc
  in
  index_intro_to_html b @@ fun raw_index_intro ->
  let pkg_infos = Conf.pkg_infos b.conf in
  let pkgs = Odig_odoc_page.pkgs_with_html_docs b.conf in
  let stamp = match raw_index_intro with None -> [] | Some s -> [s] in
  let stamp = List.fold_left (add_pkg_data pkg_infos) stamp pkgs in
  let stamp = match ocaml_manual_uri with
  | None -> stamp
  | Some s -> (s :: stamp)
  in
  let stamp = String.concat " " (odig_version :: stamp) in
  let index = Fpath.(b.html_dir / "index.html") in
  let index_title = b.index_title in
  Memo.write b.m ~stamp index @@ fun () ->
  Ok (Odig_odoc_page.pkg_list b.conf ~index_title ~raw_index_intro
        ~tag_index:b.tag_index ~ocaml_manual_uri pkgs)

let write_ocaml_manual b =
  let manual_pkg_dir = Fpath.(Conf.doc_dir b.conf / ocaml_manual_pkg) in
  let manual_index = Fpath.(manual_pkg_dir / "index.html") in
  let dst = Fpath.(Conf.html_dir b.conf / ocaml_manual_pkg) in
  match Os.File.exists manual_index |> Log.if_error ~use:false with
  | false -> None
  | true ->
      begin
        Memo.delete b.m dst @@ fun () ->
        let copy_file m ~src_root ~dst_root src =
          let dst = Fpath.reroot ~root:src_root ~dst:dst_root src in
          B00.Memo.file_ready m src;
          B00.Memo.copy m ~src dst
        in
        let src = manual_pkg_dir in
        let files = Os.Dir.fold_files ~recurse:true Os.Dir.path_list src [] in
        let files = Memo.fail_if_error b.m files in
        List.iter (copy_file b.m ~src_root:src ~dst_root:dst) files
      end;
      Some "ocaml-manual/index.html"

let rec build b ~esy_map = match Pkg.Set.choose b.r.pkgs_todo with
| exception Not_found ->
    Memo.stir ~block:true b.m;
    begin match Pkg.Set.is_empty b.r.pkgs_todo with
    | false -> build b ~esy_map
    | true ->
        let without_theme = match b.theme with None -> true | Some _ -> false in
        let html_dir = b.html_dir and build_dir = b.odoc_dir in
        B00_odoc.Support_files.write b.m ~without_theme ~html_dir ~build_dir;
        let ocaml_manual_uri = write_ocaml_manual b in
        write_pkgs_index b ~ocaml_manual_uri;
        write_theme b.conf b.m b.theme;
    end
| pkg ->
    b.r.pkgs_todo <- Pkg.Set.remove pkg b.r.pkgs_todo;
    b.r.pkgs_seen <- Pkg.Set.add pkg b.r.pkgs_seen;
    pkg_to_html b pkg ~esy_map;
    build b ~esy_map

let write_log_file c memo =
  Log.if_error ~use:() @@
  B00_ui.Memo.Log.(write (Conf.b0_log_file c) (of_memo memo))

let gen c ~force ~index_title ~index_intro ~pkg_deps ~tag_index pkgs_todo =
  Result.bind (Conf.memo c) @@ fun m ->
  let b =
    builder m c ~index_title ~index_intro ~pkg_deps ~tag_index pkgs_todo
  in
  let esy_map =
    if Conf.esy_mode conf then
      let map = esy_dep_map b in
      (* debug *)
      (*
      print_endline "--- Dependency Map: ---";
      String.Map.iter (fun k v ->
        print_endline @@ k^":";
        String.Set.iter (fun s -> print_string @@ s^", ") v;
        print_newline ())
      map;
      print_endline "--- End Dependency Map ---";
      *)
      Some map
    else None
  in
  Os.Sig_exit.on_sigint ~hook:(fun () -> write_log_file c m) @@ fun () ->
  Memo.spawn_fiber m (fun () -> build b ~esy_map);
  Memo.stir ~block:true m;
  write_log_file c m;
  Log.time (fun _ m -> m "deleting trash") begin fun () ->
    Log.if_error ~use:() (B00.Memo.delete_trash ~block:false m)
  end;
  match Memo.status b.m with
  | Ok () as v -> v
  | Error e ->
      let read_howto = Fmt.any "odig log -r " in
      let write_howto = Fmt.any "odig log -w " in
      B000_conv.Op.pp_aggregate_error ~read_howto ~write_howto () Fmt.stderr e;
      Error "Documentation might be incomplete (see: odig log -e)."

let install_theme c theme =
  Result.bind (Conf.memo c) @@ fun m ->
  Memo.spawn_fiber m (fun () -> write_theme c m theme);
  Memo.stir ~block:true m;
  match Memo.status m with
  | Ok () as v -> v
  | Error e -> Error "Could not set theme"

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The odig programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
