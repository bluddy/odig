(*---------------------------------------------------------------------------
   Copyright (c) 2018 The odig programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Odig_support
open Astring

(* Return codes and error handling *)

let err_name = 1
let err_uri = 2
let err_some = 3

let handle_error code v f = match v with
| Error e -> Logs.err (fun m -> m "%a" Fmt.lines e); code
| Ok v -> f v

let handle_name_error v f = handle_error err_name v f
let handle_some_error v f = handle_error err_some v f

(* Commonalities *)

let find_pkgs conf = function
| [] -> Ok (Conf.pkgs conf)
| ns ->
    let pkgs = Conf.pkgs conf in
    let by_name = Pkg.by_names pkgs in
    let add_name (fnd, miss) n = match String.Map.find n by_name with
    | None -> (fnd, n :: miss)
    | Some pkg -> (pkg :: fnd, miss)
    in
    let fnd, miss = List.fold_left add_name ([], []) ns in
    match miss with
    | [] -> Ok (List.rev fnd)
    | miss ->
        let exists = List.rev_map Pkg.name pkgs in
        Error (String.concat ~sep:"\n" miss)

let odoc_gen conf ~force ~index_title ~index_intro ~pkg_deps ~tag_index pkgs =
  Logs.app begin fun m ->
    m "Updating documentation, this may take some time..."
  end;
  Odig_odoc.gen conf ~force ~index_title ~index_intro ~pkg_deps ~tag_index pkgs

(* Commands *)

let conf_cmd conf = Fmt.pr "%a@." Conf.pp conf; 0

let odoc_cmd
    _odoc pkg_names index_title index_intro force no_pkg_deps no_tag_index
    conf
  =
  let pkg_deps = not no_pkg_deps in
  let tag_index = not no_tag_index in
  handle_name_error (find_pkgs conf pkg_names) @@ fun pkgs ->
  handle_some_error
    (odoc_gen conf ~force ~index_title ~index_intro ~pkg_deps ~tag_index pkgs)
  @@ fun () -> 0

let odoc_theme_cmd out_fmt action theme set_default conf =
  let list_themes conf out_fmt =
    match Odoc_theme.of_dir (Conf.sharedir conf) with
    | [] -> 0
    | ts ->
        let pp_theme = function
        | `Short -> Odoc_theme.pp_name
        | `Normal | `Long -> Odoc_theme.pp
        in
        Fmt.pr "@[<v>%a@]@." (Fmt.list (pp_theme out_fmt)) ts; 0
  in
  let default conf =
    let ts = Odoc_theme.of_dir (Conf.sharedir conf) in
    let theme = Conf.odoc_theme conf in
    Fmt.pr "%s@." theme;
    match Odoc_theme.find theme ts with
    | Error e -> Logs.warn (fun m -> m "%s" e); err_name
    | Ok _ -> 0
  in
  let set_theme conf theme set_default =
    let ts = Odoc_theme.of_dir (Conf.sharedir conf) in
    let theme = match theme with None -> Conf.odoc_theme conf | Some t -> t in
    match Odoc_theme.find theme ts with
    | Error e -> Logs.err (fun m -> m "%s" e); err_name
    | Ok t ->
        handle_some_error (Odig_odoc.set_theme conf t) @@ fun () ->
        match set_default with
        | false -> 0
        | true ->
            let name = Odoc_theme.name t in
            handle_some_error (Odoc_theme.set_user_preference name) @@
            fun () -> 0
  in
  let path conf theme =
    let ts = Odoc_theme.of_dir (Conf.sharedir conf) in
    let theme = match theme with None -> Conf.odoc_theme conf | Some t -> t in
    match Odoc_theme.find theme ts with
    | Error e -> Logs.err (fun m -> m "%s" e); err_name
    | Ok t -> Fmt.pr "%a@." Fpath.pp (Odoc_theme.path t); 0
  in
  match action with
  | `List -> list_themes conf out_fmt
  | `Default -> default conf
  | `Set -> set_theme conf theme set_default
  | `Path -> path conf theme

(* Command line interface *)

open Cmdliner

(* Arguments and commonalities *)

let exits =
  Term.exit_info err_name ~doc:"a specified entity name cannot be found." ::
  Term.exit_info err_uri ~doc:"an URI cannot be shown in a browser." ::
  Term.exit_info err_some ~doc:"indiscriminate error reported on stderr." ::
  Term.default_exits

type out_fmt = [ `Normal | `Short | `Long ]
let out_fmt =
  let short =
    let doc = "Short output. Line based output with only relevant data." in
    Arg.info ["s"; "short"] ~doc
  in
  let long =
    let doc = "Long output. Outputs as much information as possible." in
    Arg.info ["l"; "long"] ~doc
  in
  Arg.(value & vflag `Normal [`Short, short; `Long, long])

let b0_std_setup =
  let color_env = Arg.env_var "ODIG_COLOR" in
  let verbosity_env = Arg.env_var "ODIG_VERBOSITY" in
  B0_ui.Cli.B0_std.setup ~color_env ~verbosity_env ()

let conf =
  let path = B0_ui.Cli.Arg.path in
  let docs = Manpage.s_common_options in
  let docv = "PATH" in
  let doc dirname dir =
    Fmt.str
    "%s directory. If unspecified, $(b,\\$PREFIX)/%s with $(b,\\$PREFIX) \
     the parent directory of $(mname)'s install directory." dirname dir
  in
  let cachedir =
    let doc = doc "Cache" "var/cache/odig" in
    let env = Arg.env_var Conf.cachedir_env in
    Arg.(value & opt (some path) None & info ["cachedir"] ~doc ~docs ~env ~docv)
  in
  let libdir =
    let doc = doc "Library" "lib" in
    let env = Arg.env_var Conf.libdir_env in
    Arg.(value & opt (some path) None & info ["libdir"] ~doc ~docs ~env ~docv)
  in
  let docdir =
    let doc = doc "Documentation" "doc" in
    let env = Arg.env_var Conf.docdir_env in
    Arg.(value & opt (some path) None & info ["docdir"] ~doc ~docs ~env ~docv)
  in
  let sharedir =
    let doc = doc "Share" "share" in
    let env = Arg.env_var Conf.sharedir_env in
    Arg.(value & opt (some path) None & info ["sharedir"] ~doc ~docs ~env ~docv)
  in
  let odoc_theme =
    let doc = "Theme to use for odoc documentation. If unspecified, the theme \
               can be specified in the file $(b,~/.config/odig/odoc-theme) \
               or $(b,odoc.default) is used."
    in
    let env = Arg.env_var Conf.odoc_theme_env in
    Arg.(value & opt (some string) None &
         info ["odoc-theme"] ~doc ~docs ~env ~docv:"THEME")
  in
  let max_spawn =
    let env = Arg.env_var "ODIG_JOBS" in
    B0_ui.Memo.jobs ~docs ~env ()
  in
  let esy_mode =
    let doc = "Run odig on an $(b,esy) environment and interpret directories \
               for version information using esy's conventions. \
               Make sure to point the --libdir to the correct location."
    in
    let env = Arg.env_var "ODIG_ESY_MODE" in
    Arg.(value & flag & info ["esy-mode"] ~doc ~env)
  in
  let conf libdir docdir sharedir odoc_theme esy_mode max_spawn =
    match
      Conf.v ?libdir ?docdir ?sharedir ?odoc_theme ~esy_mode ~max_spawn ()
    with
    | Ok v -> `Ok v
    | Error e -> `Error (false, e)
  in
  Term.(ret @@
        (const conf $ libdir $ docdir $ sharedir $ odoc_theme $
         esy_mode $ max_spawn))

let pkgs_pos1_nonempty, pkgs_pos, pkgs_pos1, pkgs_opt =
  let doc = "Package to consider (repeatable)." in
  let docv = "PKG" in
  Arg.(non_empty & pos_right 0 string [] & info [] ~doc ~docv),
  Arg.(value & pos_all string [] & info [] ~doc ~docv),
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv),
  Arg.(value & opt_all string [] & info ["p"; "pkg"] ~doc ~docv)

(* Commands *)

let cache_cmd =
  let doc = "Operate on the odig cache" in
  let sdocs = Manpage.s_common_options and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(mname) $(tname) $(i,ACTION) [$(i,OPTION)]...";
    `S Manpage.s_description;
    `P "The $(tname) command operates on the odig cache. See the available
        actions below.";
    `S "ACTIONS";
    `I ("$(b,path)", "Display the path to the cache");
    `I ("$(b,clear)", "Clear the cache");
    `I ("$(b,trim)", "Trim the b0 cache (doesn't affect generated docs)"); ]
  in
  let action =
    let action = [ "path", `Path; "clear", `Clear; "trim", `Trim ] in
    let doc = Fmt.str "The action to perform. $(docv) must be one of %s."
        (Arg.doc_alts_enum action)
    in
    let action = Arg.enum action in
    Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")
  in
  let cmd = Term.(const cache_cmd $ action) in
  Term.(cmd $ conf),
  Term.info "cache" ~doc ~sdocs ~exits ~man ~man_xrefs

let odoc_cmd =
  let doc = "Generate odoc API documentation and manuals" in
  let sdocs = Manpage.s_common_options and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) generates the odoc API documentation and manual of packages.";
    `P "See the packaging conventions in $(mname) $(b,doc) $(mname) for
        generation details."; ]
  in
  let odoc = Term.const "odoc"
(* let doc = "The odoc command to use." in
    let env = Arg.env_var "ODIG_ODOC" in
    Arg.(value & opt string "odoc" & info ["odoc"] ~env ~docv:"CMD" ~doc) *)
  in
  let force = Term.const false
    (* let doc = "Force generation even if files are up-to-date." in
    Arg.(value & flag & info ["f"; "force"] ~doc) *)
  in
  let index_title =
    let doc = "$(docv) is the title of the package list page." in
    let docv = "TITLE" in
    Arg.(value & opt (some string) None & info ["index-title"] ~docv ~doc)
  in
  let index_intro =
    let doc = "$(docv) is the .mld file to use to define the introduction
               text on the package list page."
    in
    let some_path = Arg.some Fpath.v in
    Arg.(value & opt some_path None & info ["index-intro"] ~docv:"MLDFILE" ~doc)
  in
  let no_pkg_deps =
    let doc = "Restrict documentation generation to the packages mentioned \
               on the command line, their dependencies are not automatically \
               included in the result. Note that this may lead to broken \
               links in the documentation set."
    in
    Arg.(value & flag & info ["no-pkg-deps"] ~doc)
  in
  let no_tag_index =
    let doc = "Do not generate the tag index on the package list page." in
    Arg.(value & flag & info ["no-tag-index"] ~doc)
  in
  let cmd = Term.(const odoc_cmd $ odoc $ pkgs_pos $ index_title $ index_intro $
                  force $ no_pkg_deps $ no_tag_index)
  in
  Term.(cmd $ conf), Term.info "odoc" ~doc ~sdocs ~exits ~man ~man_xrefs

(* Main command *)

let odig =
  let doc = "Lookup documentation of installed OCaml packages" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) looks up documentation of installed OCaml packages. It shows \
        package metadata, readmes, change logs, licenses, cross-referenced \
        $(b,odoc) API documentation and manuals.";
    `P "See $(b,odig doc) $(mname) for a tutorial and more details."; `Noblank;
    `P "See $(mname) $(b,conf --help) for information about $(mname) \
        configuration.";
    `S Manpage.s_see_also;
    `P "Consult $(b,odig doc odig) for a tutorial, packaging conventions and
         more details.";
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ];
  in
  fst odoc_cmd,
  Term.info "odig" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man

let () =
  let cmds =
    [ cache_cmd; odoc_cmd; ]
  in
  Term.(exit_status @@ eval_choice odig cmds)

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
