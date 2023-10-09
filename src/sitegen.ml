(* 
    TODO list
        - check if it's a blog post
            then
            - replace <body> with <body class='blog'>
            - find the <sub> element make the pub_date 
            - find <x-blog-title> for the title
            otherwise
            - find <x-title> for the title
            - use now as pub_date

        - check if it's an index
            then
            - grab a different template = settings.templateindex
            otherwise
            - use = settings.template
        
        - grab the tags from <x-tags>
        - grab the description from <x-desc>
        - make the vanity from the current folder and append it to the settings.webroot
            for blogs posts this takes /blog/posts/ in between
        
        - replace 
            settings.titletag contents with title, 
            settings.keywordstag contents with comma separated list of tags, 
            settings.descriptiontag contents with description, 
            settings.contenttag contents with contents of markdown

        - save the html in place using save_to_html_file
 *)

let rec find_markdown_files_rec path : string list = 
    Sys.readdir path
    |> Array.to_list
    |> List.map (fun filename ->
        let full_path = (Filename.concat path filename) in
        match full_path with
        | md_file when String.ends_with ~suffix:".md" md_file -> [md_file]
        | directory when Sys.is_directory directory -> (find_markdown_files_rec directory)
        | _ -> []
    )
    |> List.flatten

and find_settings : string = 
    let home_dir = Sys.getenv "HOME" in
    let default_location = (Filename.concat home_dir "/.config/site-gen/settings.json") in
    if Sys.file_exists default_location then default_location else ".settings.json"

and base_path_from_settings file : string =
    let open Yojson.Basic.Util in 
    let json = Yojson.Basic.from_file file in 
    json |> member "workdir" |> to_string

let read_file file : string = In_channel.with_open_text file In_channel.input_all (* |> String.split_on_char '\n' *)

let save_to_html_file (filename:string) (html_content:string) =
    let out = open_out filename in
    output_string out html_content;
    close_out out


let () = 
    find_settings 
    |> base_path_from_settings 
    |> find_markdown_files_rec
    |> List.iter (fun file -> 
        read_file file 
        |> Omd.of_string 
        |> Omd.to_html 
        |> save_to_html_file "test.html" 
        (* TODO this is only saving to the same file locally over and over *)
    )
