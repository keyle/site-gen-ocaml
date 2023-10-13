(* 
    TODO list
        - generate the blog index
        - generate the sitemap
        - generate the rss feed
 *)

open Utils
open Data
open Boring

let () = 
    let settings = find_settings |> parse_settings in
    let articles =
        find_markdown_files_rec ~from_path:settings.workdir
        |> List.map (fun (article: post) -> 
            (* Printf.printf "Parsing %s \n" article.file; *)
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
                article.html 
                |> parse_selector ~selector:"x-desc";
            
            (* populate the VANITY URL = settings.webroot + (article.path - settings.workdir - article.file) *)
            article.url <-
                settings.webroot ^ article.path 
                |> string_remove ~needle:settings.workdir
                |> string_remove ~needle:article.file;

            (* page <title> *)
            article.html <- article.html |> string_replace_all ~needle:settings.titletag ~replacement:article.title;
            (* page meta <keywords> *)
            article.html <- article.html |> string_replace_all ~needle:settings.keywordstag ~replacement:(String.concat ", " article.tags);
            (* page meta <description> *)
            article.html <- article.html |> string_replace_all ~needle:settings.descriptiontag ~replacement:article.description;
            (* save contents of html into the (path - filename) + "index.html" *)
            article.html |> save_to_html_file ((string_remove ~needle:article.file article.path) ^ "index.html");

            (* print_endline (show_post article); *)
            (* print_endline (show_settings settings); *)
            article (* map returns a list of articles so lets return the article *)
            
        ) in 
            generate_blog_index settings articles;
            generate_sitemap settings articles;
            generate_rss_feed settings articles;
        

            (* articles |> List.iter (fun article -> show_post article |> print_endline) *)
            (* print_endline !contents; *)

