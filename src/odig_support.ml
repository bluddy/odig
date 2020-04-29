(*---------------------------------------------------------------------------
   Copyright (c) 2018 The odig programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

module Digest = struct
  include Digest
  let pp ppf d = Format.pp_print_string ppf (to_hex d)
  let pp_opt ppf = function
  | None -> Fmt.string ppf "--------------------------------"
  | Some d -> pp ppf d

  module Set = Set.Make (Digest)
  module Map = Map.Make (Digest)
end

module Esy = struct
  (* Capture the version from the directory string *)
  let esy_regex = Str.regexp {|^opam__s__\(.+\)-opam__c__\(.+\)-\([^-]+\)$|}
  let twounder_regex = Str.regexp {|__|}
  let xxx_regex = Str.regexp {|XXX|}
  let under_regex = Str.regexp {|_|}
  let dash_regex = Str.regexp {|-|}

  (* map from lowercase to uppercase names. supplied by user *)
  let cap_map =
    let file = Result.to_failure @@ Fpath.of_string "./opam_caps.txt" in
    match Os.File.exists file with
    | Ok true ->
        let r = Os.File.read file in
        let str = Result.value r ~default:"" in
        let str_l = String.split_on_char '\n' str in
        List.fold_left (fun map s ->
          String.Map.add (String.Ascii.lowercase s) s map)
          String.Map.empty
          str_l
    | _ -> String.Map.empty

  let long_name_of_pkg name ver subver =
    let name =
      Str.global_replace under_regex "XXX" name |>
      Str.global_replace dash_regex "_" |>
      Str.global_replace xxx_regex "__"
    in
    Printf.sprintf
    "opam__s__%s-opam__c__%s-%s"
      (String.Ascii.lowercase name)
      ver
      subver

  let name_ver_of_long_name name =
    (* Extract version, subversion from esy directory name *)
    try
      let _ = Str.search_forward esy_regex name 0 in
      let version = Str.matched_group 2 name in
      let subversion = Str.matched_group 3 name in
      let name_s = Str.matched_group 1 name in
      (* Double underscores become underscores. Single become dashes *)
      let name_s =
        Str.global_replace twounder_regex "XXX" name_s |>
        Str.global_replace under_regex "-" |>
        Str.global_replace xxx_regex "_"
      in
      let name_s =
        match String.Map.find_opt name_s cap_map with
        | None -> name_s
        | Some cap_name -> cap_name
      in
      name_s, version, subversion
    with Not_found ->
      invalid_arg @@ "Cannot convert long_name " ^ name
end

module Pkg = struct
  type t = {
    name : string;
    version : (string * string) option; (* version * subversion *)
    path : Fpath.t;
  }
  let name x = x.name
  let version x = x.version
  let path x = x.path
  let v ?version name path = {name; version; path}

  let out_dirname ?(subver=true) x = match x.version with
    | Some (v, s) when subver -> Fmt.str "%s.%s-%s" x.name v s
    | Some (v, _) -> Fmt.str "%s.%s" x.name v
    | _ -> x.name

  let pp ppf x = Fmt.pf ppf "%s %a" (name x)
    (Fmt.tty [`Faint] Fpath.pp_quoted) (path x)
  let pp_name ppf x = Fmt.string ppf (name x)
  let pp ppf (n, p) = Fmt.pf ppf "%s %a" n (Fmt.tty [`Faint] Fpath.pp_quoted) p
  let pp_name ppf (n, p) = Fmt.string ppf n
  let pp_version ppf v =
    let v = if v = "" then "?" else v in
    Fmt.pf ppf "%a" (Fmt.tty_string [`Fg `Green]) v

  let equal = ( = )
  let compare = compare
  let compare_by_caseless_name p p' =
    let n p = String.Ascii.lowercase (name p) in
    String.compare (n p) (n p')

  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  let of_dir ~esy_mode dir =
    Log.time (fun _ m -> m "package list of %a" Fpath.pp_quoted dir) @@
    fun () ->
    let ocaml_pkg () =
      let ocaml_where = Cmd.(arg "ocamlc" % "-where") in
      let p = Os.Cmd.run_out ~trim:true ocaml_where |> Result.to_failure in
      v "ocaml" (Fpath.of_string p |> Result.to_failure)
    in
    try
      let add_pkg _ name dir acc =
        if esy_mode then begin
          try
            let prefix = String.sub name 0 4 in
            if prefix <> "opam" then acc
            else
              (* Extract version, subversion from esy directory name *)
              let name_s, ver, subver = Esy.name_ver_of_long_name name in
              let lib_dir = Fpath.(dir / "lib") in
              (* Not reliable for opam package name *)
              let subdirs =
                Os.Dir.fold_dirs ~recurse:false
                (fun _ name _ acc -> name::acc) lib_dir []
                |> Result.to_failure
              in
              let dir_name_s =
                match subdirs with
                | [] -> raise Not_found
                | x::_ -> x
              in
              let install_dir = Fpath.(lib_dir / dir_name_s) in
              (v ~version:(ver, subver) name_s install_dir) :: acc
          with Not_found -> acc
        end else
          if name = "ocaml" then acc else (v name dir) :: acc
      in
      let pkgs = Os.Dir.fold_dirs ~recurse:false add_pkg dir [] in
      let pkgs = pkgs |> Result.to_failure in
      List.sort compare_by_caseless_name (ocaml_pkg () :: pkgs)
    with Failure e -> Log.err (fun m -> m "package list: %s" e); []

  let by_names ?(init = String.Map.empty) ?(use_dirname=false) ?(subver=false) pkgs =
    let get_name = if use_dirname then out_dirname ~subver else name in
    let add_pkg acc pkg = String.Map.add (get_name pkg) pkg acc in
    List.fold_left add_pkg init pkgs
end

module Doc_cobj = struct
  type kind = Cmi | Cmti | Cmt
  type t =
    { path : Fpath.t;
      kind : kind;
      modname : string;
      hidden : bool;
      pkg : Pkg.t; }

  let path cobj = cobj.path
  let kind cobj = cobj.kind
  let modname cobj = cobj.modname
  let pkg cobj = cobj.pkg
  let hidden cobj = cobj.hidden
  let don't_list cobj =
    hidden cobj || String.is_infix ~affix:"__" (modname cobj)

  let add_cobj pkg _ _ path acc =
    try
      let multi = true in (* implies e.g .p.ext objects are not considered *)
      let base, kind = match Fpath.cut_ext ~multi path with
      | base, ".cmi" -> base, Cmi
      | base, ".cmti" -> base, Cmti
      | base, ".cmt" -> base, Cmt
      | base, _ -> raise_notrace Exit
      in
      let modname = String.Ascii.capitalize (Fpath.basename base) in
      let cobj = match Fpath.Map.find base acc with
      | exception Not_found ->
          let hidden = not (kind = Cmi) in
          { path; kind; modname; hidden; pkg; }
      | cobj' ->
          match cobj'.kind, kind with
          | Cmi, (Cmti | Cmt) -> { path; kind; modname; hidden = false; pkg;  }
          | (Cmti | Cmt), Cmi -> { cobj' with hidden = false }
          | Cmt, Cmti -> { path; kind; modname; hidden = cobj'.hidden; pkg }
          | Cmti, Cmt | _ -> cobj'
      in
      Fpath.Map.add base cobj acc
    with Exit -> acc

  let of_pkg pkg =
    let dir = Pkg.path pkg in
    let recurse = true in
    let cobjs = Os.Dir.fold_files ~recurse (add_cobj pkg) dir Fpath.Map.empty in
    let cobjs = Log.if_error ~use:Fpath.Map.empty cobjs in
    Fpath.Map.fold (fun _ c acc -> c :: acc) cobjs []

  let by_modname ?(init = String.Map.empty) cobjs =
    let add acc cobj = match String.Map.find cobj.modname acc with
    | exception Not_found -> String.Map.add cobj.modname [cobj] acc
    | cobjs -> String.Map.add cobj.modname (cobj :: cobjs) acc
    in
    List.fold_left add init cobjs
end

module Opam = struct

  (* opam metadata *)

  type t = (string * string) list

  let unescape s = s (* TODO *)
  let parse_string = function
  | "" -> ("", "")
  | s ->
      match String.index s '"' with
      | exception Not_found -> (s, "")
      | i ->
          let start = i + 1 in
          let rec find_end i = match String.index_from s i '"' with
          | exception Not_found -> (s, "") (* unreported error ... *)
          | j when s.[j - 1] = '\\' -> find_end (j + 1)
          | j ->
              let stop = j - 1 in
              let str = String.sub s start (stop - start + 1) in
              let rest = String.sub s (j + 1) (String.length s - (j + 1)) in
              (unescape str, rest)
          in
          find_end start

  let parse_list s =
    if s = "" then [] else
    let rec loop acc s =
      let s, rest = parse_string s in
      let rest = String.trim rest in
      if rest = "" || rest = "]" then List.rev (s :: acc) else
      loop (s :: acc) rest
    in
    loop [] s

  let string_field f fields = match List.assoc f fields with
  | exception Not_found -> "" | v -> fst @@ parse_string v

  let list_field ?(sort = true) f fields = match List.assoc f fields with
  | exception Not_found -> []
  | v when sort -> List.sort compare (parse_list v)
  | v -> parse_list v

  let authors = list_field "authors"
  let bug_reports = list_field "bug-reports"
  let depends fs = match List.assoc "depends" fs with
  | exception Not_found -> [] | v ->
      let delete_constraints s =
        let rec loop s = match String.index s '{' with
        | exception Not_found -> s
        | i ->
            match String.index s '}' with
            | exception Not_found -> s
            | j ->
                loop (String.sub s 0 i) ^
                loop (String.sub s (j + 1) (String.length s - (j + 1)))
        in
        loop s
      in
      List.sort compare @@ parse_list (delete_constraints v)

  let dev_repo = list_field "dev-repo"
  let doc = list_field "doc"
  let homepage = list_field "homepage"
  let license = list_field "license"
  let maintainer = list_field "maintainer"
  let synopsis = string_field "synopsis"
  let tags fs = List.rev_map String.Ascii.lowercase @@ list_field "tags" fs
  let version = string_field "version"

  (* Queries *)

  let file pkg =
    let opam = Fpath.(Pkg.path pkg / "opam") in
    match Os.File.exists opam |> Log.if_error ~use:false with
    | true -> Some opam
    | false -> None

  let bin = lazy begin
    Result.bind (Os.Cmd.must_find_tool (Fpath.v "opam")) @@ fun opam ->
    Result.bind (Os.Cmd.run_out ~trim:true Cmd.(path opam % "--version")) @@
    fun v -> match String.cut_left ~sep:"." (String.trim v) with
    | Some (maj, _)  when
        maj <> "" && Char.code maj.[0] - 0x30 >= 2 -> Ok opam
    | Some _ | None ->
        Fmt.error "%a: unsupported version %s" Fpath.pp_quoted opam v
  end

  let fields =
    [ "name:"; "authors:"; "bug-reports:"; "depends:"; "dev-repo:"; "doc:";
      "homepage:"; "license:"; "maintainer:"; "synopsis:"; "tags:";
      "version:" ]

  let field_count = List.length fields
  let field_arg = Fmt.str "--field=%s" (String.concat "," fields)
  let rec take_fields n acc lines = match n with
  | 0 -> acc, lines
  | n ->
      match lines with
      | [] -> [], [] (* unreported error... *)
      | l :: ls ->
          match String.cut_left ~sep:":" l with
          | None -> [], [] (* unreported error... *)
          | Some (f, v) -> take_fields (n - 1) ((f, String.trim v) :: acc) ls

  let rec parse_lines acc = function
  | [] -> acc
  | name :: lines ->
      let err l =
        Log.err (fun m -> m "%S: opam metadata expected name: field line" l)
      in
      match String.cut_left ~sep:":" name with
      | Some ("name", n) ->
          let n, _ = parse_string n in
          let fields, lines = take_fields (field_count - 1) [] lines in
          parse_lines (String.Map.add n fields acc) lines
      | None | Some _ -> err name; acc

  let query qpkgs ~esy_mode =
    if esy_mode then
      (* For esy_mode, we want to use just the name of the package *)
      let pkgs = Pkg.Set.of_list qpkgs |> Pkg.Set.elements in
      let opams = List.map (fun p -> Pkg.out_dirname ~subver:false p) pkgs in
      let no_data pkgs = List.map (fun p -> (p, [])) pkgs in
      match Lazy.force bin with
      | Error e -> Log.err (fun m -> m "%s" e); no_data qpkgs
      | Ok opam ->
          if opams = [] then no_data qpkgs else
          let show = Cmd.(path opam % "show" % "--normalise" % "--no-lint") in
          (* TODO: OPAM can't handle multiple packages of the same lib
          * with different versions.
          * For now, we'll invoke the command once per package *)
          let show pkg = Cmd.(show % field_arg % pkg) in
          let opam_string pkg =
            match
              Log.time (fun _ m -> m "opam show") @@ fun () ->
              let stderr = `Stdo (Os.Cmd.out_null) in
              Os.Cmd.run_out ~stderr (show pkg)
            with
            | Error e -> Log.err (fun m -> m "%s" e); ""
            | Ok out -> out
          in
          let str = List.map opam_string opams |> String.concat "\n" in
          let lines = String.cuts_left ~sep:"\n" str in
          let infos = parse_lines String.Map.empty lines in
          let find_info is p = match String.Map.find (Pkg.name p) is with
            | exception Not_found -> p, []
            | i -> p, i
          in
          try List.map (find_info infos) qpkgs with
          | Not_found -> assert false
    else
      let pkgs = Pkg.Set.of_list qpkgs in
      let add_opam p acc = match file p with None -> acc | Some f -> f :: acc in
      let opams = Pkg.Set.fold add_opam pkgs [] in
      let no_data pkgs = List.map (fun p -> (p, [])) pkgs in
      match Lazy.force bin with
      | Error e -> Log.err (fun m -> m "%s" e); no_data qpkgs
      | Ok opam ->
          if opams = [] then no_data qpkgs else
          let show = Cmd.(path opam % "show" % "--normalise" % "--no-lint") in
          (* TODO: OPAM can't handle multiple packages of the same lib
          * with different versions.
          * For now, we'll invoke the command once per package *)
          let show pkg = Cmd.(show % field_arg %% path pkg) in
          let opam_string pkg =
            match
              Log.time (fun _ m -> m "opam show") @@ fun () ->
              let stderr = `Stdo (Os.Cmd.out_null) in
              Os.Cmd.run_out ~stderr (show pkg)
            with
            | Error e -> Log.err (fun m -> m "%s" e); ""
            | Ok out -> out
          in
          let str = List.map opam_string opams |> String.concat "\n" in
          let lines = String.cuts_left ~sep:"\n" str in
          let infos = parse_lines String.Map.empty lines in
          let find_info is p = match String.Map.find (Pkg.name p) is with
            | exception Not_found -> p, []
            | i -> p, i
          in
          try List.map (find_info infos) qpkgs with
          | Not_found -> assert false
end

module Doc_dir = struct

  (* Doc dir info *)

  type files =
    { changes_files : Fpath.t list;
      license_files : Fpath.t list;
      readme_files : Fpath.t list; }

  type t =
    { dir : Fpath.t option;
      files : files Lazy.t;
      odoc_pages : Fpath.t list Lazy.t;
      odoc_assets_dir : Fpath.t option Lazy.t;
      odoc_assets : Fpath.t list Lazy.t; }

  let doc_dir_files pkg_doc_dir =
    let cs, ls, rs = match pkg_doc_dir with
    | None -> [], [], []
    | Some doc_dir ->
        let add_file _ base file (cs, ls, rs as acc) =
          let base = String.uppercase_ascii base in
          let is_pre pre = String.is_prefix pre base in
          if is_pre "CHANGE" || is_pre "HISTORY" || is_pre "NEWS"
          then (file :: cs), ls, rs else
          if is_pre "LICENSE" then cs, (file :: ls), rs else
          if is_pre "README" then cs, ls, (file :: rs) else
          acc
        in
        Os.Dir.fold_files ~recurse:false add_file doc_dir ([], [], [])
        |> Log.if_error ~use:([], [], [])
    in
    let changes_files = List.sort Fpath.compare cs in
    let license_files = List.sort Fpath.compare ls in
    let readme_files = List.sort Fpath.compare rs in
    { changes_files; license_files; readme_files }

  let doc_dir_subdir_files pkg_doc_dir sub ~sat = match pkg_doc_dir with
  | None -> []
  | Some pkg_doc_dir ->
      let dir = Fpath.(pkg_doc_dir / sub) in
      match Os.Dir.exists dir with
      | Ok false | Error _  -> []
      | Ok true ->
          let add_file = match sat with
          | None -> fun _ _ file acc -> file :: acc
          | Some sat ->
              fun _ _ file acc -> if sat file then file :: acc else acc
          in
          Os.Dir.fold_files ~recurse:true add_file dir []
          |> Log.if_error ~use:[]

  let doc_dir_odoc_pages pkg_doc_dir =
    let is_mld = Some (Fpath.has_ext ".mld") in
    doc_dir_subdir_files pkg_doc_dir "odoc-pages" ~sat:is_mld

  let doc_dir_odoc_assets pkg_doc_dir  =
    doc_dir_subdir_files pkg_doc_dir "odoc-assets" ~sat:None

  let doc_dir_odoc_assets_dir pkg_doc_dir = match pkg_doc_dir with
  | None -> None
  | Some pkg_doc_dir ->
      let dir = Fpath.(pkg_doc_dir / "odoc-assets") in
      match Os.Dir.exists dir |> Log.if_error ~use:false with
      | false -> None
      | true -> Some dir

  let v pkg_doc_dir =
    let files = lazy (doc_dir_files pkg_doc_dir) in
    let odoc_pages = lazy (doc_dir_odoc_pages pkg_doc_dir) in
    let odoc_assets_dir = lazy (doc_dir_odoc_assets_dir pkg_doc_dir) in
    let odoc_assets = lazy (doc_dir_odoc_assets pkg_doc_dir) in
    { dir = pkg_doc_dir; files; odoc_pages; odoc_assets_dir; odoc_assets }

  let dir i = i.dir
  let changes_files i = (Lazy.force i.files).changes_files
  let license_files i = (Lazy.force i.files).license_files
  let odoc_pages i = Lazy.force i.odoc_pages
  let odoc_assets_dir i = Lazy.force i.odoc_assets_dir
  let odoc_assets i = Lazy.force i.odoc_assets
  let readme_files i = (Lazy.force i.files).readme_files
  let of_pkg ~doc_dir pkg =
    let doc_dir = Fpath.(doc_dir / Pkg.name pkg) in
    match Os.Dir.exists doc_dir |> Log.if_error ~use:false with
    | true -> v (Some doc_dir)
    | false -> v None
end

module Pkg_info = struct
  type t =
    { doc_cobjs : Doc_cobj.t list Lazy.t;
      opam : Opam.t;
      doc_dir : Doc_dir.t Lazy.t }

  let doc_cobjs i = Lazy.force i.doc_cobjs
  let opam i = i.opam
  let doc_dir i = Lazy.force i.doc_dir

  type field =
  [ `Authors | `Changes_files | `Doc_cobjs | `Depends | `Homepage | `Issues
  | `License | `License_files | `Maintainers | `Odoc_assets | `Odoc_pages
  | `Online_doc | `Readme_files | `Repo | `Synopsis | `Tags | `Version ]

  let field_names =
    [ "authors", `Authors; "changes-files", `Changes_files;
      "depends", `Depends; "doc-cobjs", `Doc_cobjs;
      "homepage", `Homepage; "issues", `Issues; "license", `License;
      "license-files", `License_files; "maintainers", `Maintainers;
      "odoc-assets", `Odoc_assets; "odoc-pages", `Odoc_pages;
      "online-doc", `Online_doc; "readme-files", `Readme_files;
      "repo", `Repo; "synopsis", `Synopsis; "tags", `Tags;
      "version", `Version; ]

  let get field i =
    let paths ps = List.map Fpath.to_string ps in
    match field with
    | `Authors -> Opam.authors (opam i)
    | `Changes_files -> paths @@ Doc_dir.changes_files (doc_dir i)
    | `Depends -> Opam.depends (opam i)
    | `Doc_cobjs -> paths @@ List.map Doc_cobj.path (doc_cobjs i)
    | `Homepage -> Opam.homepage (opam i)
    | `Issues -> Opam.bug_reports (opam i)
    | `License -> Opam.license (opam i)
    | `License_files -> paths @@ Doc_dir.license_files (doc_dir i)
    | `Maintainers -> Opam.maintainer (opam i)
    | `Odoc_assets -> paths @@ Doc_dir.odoc_assets (doc_dir i)
    | `Odoc_pages -> paths @@ Doc_dir.odoc_pages (doc_dir i)
    | `Online_doc -> Opam.doc (opam i)
    | `Readme_files -> paths @@ Doc_dir.readme_files (doc_dir i)
    | `Repo -> Opam.dev_repo (opam i)
    | `Synopsis -> (match Opam.synopsis (opam i) with "" -> [] | s -> [s])
    | `Tags -> Opam.tags (opam i)
    | `Version -> (match Opam.version (opam i) with "" -> [] | s -> [s])

  let pp ppf i =
    let pp_value = Fmt.(hvbox @@ list ~sep:sp string) in
    let pp_field ppf (n, f) = Fmt.field n (get f) pp_value ppf i in
    let pp_field ppf spec = Fmt.pf ppf "| %a" pp_field spec in
    Fmt.pf ppf "@[<v>%a@]" (Fmt.list pp_field) field_names

  (* Queries *)

  let query ~doc_dir ~esy_mode pkgs =
    let rec loop acc = function
    | [] -> List.rev acc
    | (p, opam) :: ps ->
        let doc_cobjs = lazy (Doc_cobj.of_pkg p) in
        let doc_dir = lazy (Doc_dir.of_pkg ~doc_dir p) in
        loop ((p, {doc_cobjs; opam; doc_dir}) :: acc) ps
    in
    loop [] (Opam.query ~esy_mode pkgs)
end

module Env = struct
  let b0_cache_dir = "ODIG_B0_CACHE_DIR"
  let b0_log_file = "ODIG_B0_LOG_FILE"
  let cache_dir = "ODIG_CACHE_DIR"
  let color = "ODIG_COLOR"
  let doc_dir = "ODIG_DOC_DIR"
  let lib_dir = "ODIG_LIB_DIR"
  let odoc_theme = "ODIG_ODOC_THEME"
  let share_dir = "ODIG_SHARE_DIR"
  let verbosity = "ODIG_VERBOSITY"
end

module Conf = struct

  type t =
    { b0_cache_dir : Fpath.t;
      b0_log_file : Fpath.t;
      cache_dir : Fpath.t;
      cwd : Fpath.t;
      doc_dir : Fpath.t;
      html_dir : Fpath.t;
      jobs : int;
      lib_dir : Fpath.t;
      log_level : Log.level;
      memo : (B00.Memo.t, string) result Lazy.t;
      odoc_theme : string;
      esy_mode : bool;
      pkg_infos : Pkg_info.t Pkg.Map.t Lazy.t;
      pkgs : Pkg.t list Lazy.t;
      share_dir : Fpath.t;
      tty_cap : Tty.cap; }

  let memo ~cwd ~cache_dir (* b0 not odig *) ~trash_dir ~jobs =
    let feedback =
      let op_howto ppf o = Fmt.pf ppf "odig log --id %d" (B000.Op.id o) in
      let show_op = Log.Debug and show_ui = Log.Info and level = Log.level () in
      B00_ui.Memo.pp_leveled_feedback ~op_howto ~show_op ~show_ui ~level
        Fmt.stderr
    in
    B00.Memo.memo ~cwd ~cache_dir ~trash_dir ~jobs ~feedback ()

  let v
      ~b0_cache_dir ~b0_log_file ~cache_dir ~cwd ~doc_dir ~html_dir ~jobs
      ~lib_dir ~log_level ~odoc_theme ~share_dir ~tty_cap ~esy_mode ()
    =
    let trash_dir =
      B00_ui.Memo.get_trash_dir ~cwd ~b0_dir:cache_dir ~trash_dir:None
    in
    let memo =
      lazy (memo ~cwd:cache_dir ~cache_dir:b0_cache_dir ~trash_dir ~jobs)
    in
    let pkgs = lazy (Pkg.of_dir lib_dir) in
    let pkg_infos = Lazy.from_fun @@ fun () ->
      let add acc (p, i) = Pkg.Map.add p i acc in
      let pkg_infos = Pkg_info.query doc_dir (Lazy.force pkgs) in
      List.fold_left add Pkg.Map.empty pkg_infos
    in
    { b0_cache_dir; b0_log_file; cache_dir; cwd; doc_dir; html_dir; jobs;
      lib_dir; log_level; memo; odoc_theme; pkg_infos; pkgs; share_dir;
      tty_cap; esy_mode }

  let b0_cache_dir c = c.b0_cache_dir
  let b0_log_file c = c.b0_log_file
  let cache_dir c = c.cache_dir
  let cwd c = c.cwd
  let doc_dir c = c.doc_dir
  let html_dir c = c.html_dir
  let jobs c = c.jobs
  let lib_dir c = c.lib_dir
  let log_level c = c.log_level
  let memo c = Lazy.force c.memo
  let odoc_theme c = c.odoc_theme
  let pkg_infos c = Lazy.force c.pkg_infos
  let pkgs c = Lazy.force c.pkgs
  let share_dir c = c.share_dir
  let tty_cap c = c.tty_cap
  let esy_mode c = c.esy_mode
  let pp =
    Fmt.record @@
    [ Fmt.field "b0-cache-dir" b0_cache_dir Fpath.pp_quoted;
      Fmt.field "b0-log-file" b0_log_file Fpath.pp_quoted;
      Fmt.field "cache-dir" cache_dir Fpath.pp_quoted;
      Fmt.field "doc-dir" doc_dir Fpath.pp_quoted;
      Fmt.field "lib-dir" lib_dir Fpath.pp_quoted;
      Fmt.field "jobs" jobs Fmt.int;
      Fmt.field "odoc-theme" odoc_theme Fmt.string;
      Fmt.field "share-dir" share_dir Fpath.pp_quoted; ]

  (* Setup *)

  let get_dir ~cwd ~exec default_dir = function
  | Some dir -> Fpath.(cwd // dir)
  | None ->
      (* relocation hack find directory relative to executable path *)
      Fpath.((parent @@ parent @@ exec) // default_dir)

  let get_odoc_theme = function
  | Some v -> Ok v
  | None ->
      Result.bind (B00_odoc.Theme.get_user_preference ()) @@ fun n ->
      Ok (Option.value ~default:B00_odoc.Theme.odig_default n)

  let setup_with_cli
      ~b0_cache_dir ~b0_log_file ~cache_dir ~doc_dir ~jobs ~lib_dir ~log_level
      ~odoc_theme ~share_dir ~tty_cap ()
    =
    Result.map_error (Fmt.str "conf: %s") @@
    let tty_cap = B00_std_ui.get_tty_cap tty_cap in
    let log_level = B00_std_ui.get_log_level log_level in
    B00_std_ui.setup tty_cap log_level ~log_spawns:Log.Debug;
    Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
    Result.bind (Fpath.of_string Sys.executable_name) @@ fun exec ->
    let cache_dir = get_dir ~cwd ~exec (Fpath.v "var/cache/odig") cache_dir in
    let b0_cache_dir =
      let b0_dir = cache_dir and cache_dir = b0_cache_dir in
      B00_ui.Memo.get_cache_dir ~cwd ~b0_dir ~cache_dir
    in
    let b0_log_file =
      let b0_dir = cache_dir and log_file = b0_log_file in
      B00_ui.Memo.get_log_file ~cwd ~b0_dir ~log_file
    in
    let html_dir = Fpath.(cache_dir / "html") in
    let lib_dir = get_dir ~cwd ~exec (Fpath.v "lib") lib_dir in
    let doc_dir = get_dir ~cwd ~exec (Fpath.v "doc") doc_dir in
    let share_dir = get_dir ~cwd ~exec (Fpath.v "share") share_dir in
    Result.bind (get_odoc_theme odoc_theme) @@ fun odoc_theme ->
    let jobs = B00_ui.Memo.get_jobs ~jobs in
    Ok (v ~b0_cache_dir ~b0_log_file ~cache_dir ~cwd ~doc_dir ~html_dir
          ~jobs ~lib_dir ~log_level ~odoc_theme ~share_dir ~tty_cap ())
end

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
