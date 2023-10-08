
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

let () = 
    find_settings 
    |> base_path_from_settings 
    |> find_markdown_files_rec
    |> List.iter (fun file -> 
        print_endline (read_file file)
    )
