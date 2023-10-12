(* 
    TODO list
        - replace 
            settings.titletag contents with title, 
            settings.keywordstag contents with comma separated list of tags, 
            settings.descriptiontag contents with description, 
            settings.contenttag contents with contents of markdown

        - save the html in place using save_to_html_file
 *)

type post = {
    path: string;
    file: string;
    mutable markdown: string;
    mutable html: string;
    mutable title: string;
    mutable is_blog: bool;
    mutable url: string;
    mutable vanity_url: string;
    mutable pub_date: string;
    mutable description: string;
    mutable tags: string list;
} 
(* [@@deriving show] note overriden manually below *)

let show_post { title; is_blog; path; file; vanity_url; _ } = 
    Printf.sprintf "post { title: %s; is_blog: %b; path: %s; file: %s; \nvanity: %s}" title is_blog path file vanity_url

let default_post = {
    path = "";
    file = "";
    markdown = "";
    html = "";
    title = "";
    is_blog = false;
    url = "";
    vanity_url = "";
    pub_date = "";
    description = "";
    tags = [];
}

type settings = {
    workdir: string;
    webroot: string;
    template: string;
    templateindex: string;
    contenttag: string;
    titletag: string;
    descriptiontag: string;
    keywordstag: string
} [@@deriving show]

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

let () = 
        find_settings 
        |> parse_settings
        |> fun settings -> find_markdown_files_rec ~from_path:settings.workdir
        |> List.iter (fun (article: post) -> 
            Printf.printf "Parsing %s \n" article.file;
            (* populate the MARKDOWN *)
            let markdown = read_file article.path in 
                article.markdown <- markdown;
            (* determine if this page is a blog post *)
            let is_blog = string_contains ~needle:"<x-blog-title>" markdown in
                article.is_blog <- is_blog;
            (* populate TEMPLATE and use a different base template if it's an index page or content page *)
            let is_index = string_contains ~needle:"<x-index/>" markdown in
                let template_path = if is_index then settings.templateindex else settings.template in
                let template = read_file template_path in
                article.html <- template;
            (* try convert and place new HTML in place of contents tag in template *)
            let converted_html = article.markdown |> Omd.of_string |> Omd.to_html in
                article.html <- string_replace_all ~needle:settings.contenttag ~replacement:converted_html article.html;

            (* place the new TITLE in place of title tag, with the tag name varying based on the type of post *)
            let title_tag = if article.is_blog then "x-blog-title" else "x-title" in
                let title = parse_selector ~selector:title_tag article.markdown in
                article.title <- title;
                article.html <- string_replace_all ~needle:settings.titletag ~replacement:title article.html;
            (* set the CLASS 'blog' to our body tag if we're a blog post for different styling *)
            if article.is_blog then
                begin
                    article.html <- string_replace_all ~needle:"<body>" ~replacement:"<body class='blog'>" article.html;
                    article.pub_date <- article.markdown |> parse_selector ~selector:"sub"
                end
            else
                article.pub_date <- now_formatted_dt;
            (* parse and get TAGS/KEYWORDS, both used in blog posts and content pages as meta keywords *)
            article.tags <- 
                article.markdown 
                |> parse_selector ~selector:"x-tags" 
                |> String.split_on_char ',' 
                |> List.map String.trim;
            
            (* parse and get the DESCRIPTION tag, used for meta description *)
            article.description <-
                article.markdown 
                |> parse_selector ~selector:"x-desc";
            
            (* populate the VANITY URL = settings.webroot + (article.path - settings.workdir - article.file) *)
            article.vanity_url <-
                settings.webroot ^ article.path 
                |> string_remove ~needle:settings.workdir
                |> string_remove ~needle:article.file;


            (* print_endline (show_post article); *)
            (* print_endline (show_settings settings); *)

            (* |> save_to_html_file "test.html"  *)
            (* note this is only saving to the same file locally over and over *)
    )
