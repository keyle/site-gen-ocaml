open Data

let rec find_markdown_files_rec ~from_path : post list = 
    Sys.readdir from_path
    |> Array.to_list
    |> List.map (fun filename ->
        let full_path = (Filename.concat from_path filename) in
        match full_path with
        | md_file when String.ends_with ~suffix:".md" md_file -> [{default_post with path = full_path; file = filename}]
        | directory when Sys.is_directory directory -> (find_markdown_files_rec ~from_path:directory)
        | _ -> []
    )
    |> List.flatten

let find_settings : string = 
    let home_dir = Sys.getenv "HOME" in
    (* note we try to find the settings from ~.config first, if not, as a local dot file *)
    let default_location = (Filename.concat home_dir "/.config/site-gen/settings.json") in
    if Sys.file_exists default_location then default_location else ".settings.json"

let parse_settings file : settings =
	let open Yojson.Basic.Util in 
	let json = Yojson.Basic.from_file file in
	{   (* deserialize all the json settings fields to a struct *)
        workdir         = json |> member "workdir" |> to_string;
        webroot         = json |> member "webroot" |> to_string;
        template        = json |> member "template" |> to_string;
        templateindex   = json |> member "templateindex" |> to_string;
        contenttag      = json |> member "contenttag" |> to_string;
        titletag        = json |> member "titletag" |> to_string;
        descriptiontag  = json |> member "descriptiontag" |> to_string;
        keywordstag     = json |> member "keywordstag" |> to_string;
    }

let read_file file : string = In_channel.with_open_text file In_channel.input_all

let save_to_html_file (filename:string) (html_content:string) =
    let out = open_out filename in
    output_string out html_content;
    close_out out

let string_contains ~needle haystack =
    try ignore (Str.search_forward (Str.regexp_string needle) haystack 0); true
    with Not_found -> false

let now_formatted_dt = (* now as YYYY-MM-DD mm:ss *)
    let current_time = Unix.gettimeofday () in
    let tm = Unix.localtime current_time in
        Printf.sprintf "%04d-%02d-%02d %02d:%02d"
            (tm.Unix.tm_year + 1900)
            (tm.Unix.tm_mon + 1)
            tm.Unix.tm_mday
            tm.Unix.tm_hour
            tm.Unix.tm_min

let string_replace_all ~needle ~replacement haystack : string  =
    let escape_backreferences s = Str.global_replace (Str.regexp "\\\\\\([1-9][0-9]*\\)") "\\\\\\\\\\1" s in (* \1 issue in html content *)
    let escaped_replacement = escape_backreferences replacement in
    Str.global_replace (Str.regexp_string needle) escaped_replacement haystack

let string_remove ~(needle:string) haystack : string = 
    string_replace_all ~needle:needle ~replacement:"" haystack

(* selector e.g. "x-desc", note we could raise an error or return an option, I opted for the simplest, as I had no need for that *)
let parse_selector ~(selector:string) (source:string) : string =
    let open Str in
    let regex = regexp (Printf.sprintf "<%s>\\(.*\\)</%s>" selector selector) in
    try
        let _ = search_forward regex source 0 in
        matched_group 1 source
    with
    | Not_found -> "" 