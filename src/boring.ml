open Data
open Utils

let generate_blog_index (settings:settings) (articles: post list) =
    let index_html = read_file (settings.workdir ^ "/index.html") in
    let contents: string ref = ref "<table>" in
    let sorted = articles 
                |> List.filter (fun x -> x.is_blog) 
                |> List.sort (fun a b -> String.compare b.pub_date a.pub_date)
    in 
        sorted |> List.iter (fun (article:post) -> 
            contents := !contents ^ Printf.sprintf  "<tr><td>%s</td><td><a href='%s'>%s</a></td><td>&nbsp;</td>" (convert_date article.pub_date) article.url article.title
            );
        contents := !contents ^ "</table>";
    
    index_html 
        |> string_replace_all ~needle:"<x-blog-index/>" ~replacement:!contents 
        |> save_to_html_file (settings.workdir ^ "/index.html");;
            