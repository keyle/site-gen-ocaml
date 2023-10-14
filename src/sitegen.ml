
open Utils
open Data
open Boring

let () = 
    let settings = find_settings |> parse_settings in
    let articles = find_markdown_files_rec ~from_path:settings.workdir 
                |> List.map (fun article -> process_article settings article) 
    in 
        generate_blog_index settings articles;
        generate_sitemap settings articles;
        generate_rss_feed settings articles;

