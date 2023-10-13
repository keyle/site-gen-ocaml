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

let generate_sitemap (settings:settings) (articles: post list) =
    let contents: string ref = ref "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\"
  xmlns:xhtml=\"http://www.w3.org/1999/xhtml\">" in
    
        articles |> List.iter (fun (article:post) -> 
            contents := !contents ^ Printf.sprintf  "<url><loc>%s</loc><lastmod>%s</lastmod></url>\n" article.url (only_date article.pub_date)
            );
        contents := !contents ^ "</urlset>\n";
        !contents |> save_to_html_file (settings.workdir ^ "/sitemap.xml");;

let generate_rss_feed (settings:settings) (articles: post list) =
    let contents: string ref = ref "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>
<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">
  <channel>
    <title>NobenLog</title>
    <link>https://noben.org/blog/</link>
    <description>Recent content on NobenLog</description>
    <generator>site-gen-ocaml -- https://github.com/keyle/site-gen-ocaml</generator>
    <language>en-us</language>" in
        let sorted = articles 
                |> List.filter (fun x -> x.is_blog) 
                |> List.sort (fun a b -> String.compare b.pub_date a.pub_date)
            in
        sorted |> List.iter (fun (article:post) -> 
            contents := !contents ^ Printf.sprintf  "<item><title>%s</title><link>%s</link><pubDate>%s</pubDate><guid>%s</guid><description><![CDATA[ %s ]]></description></item>\n" article.title article.url article.pub_date article.url article.description
            );
        contents := !contents ^ "</channel></rss>\n";
        !contents |> save_to_html_file (settings.workdir ^ "/index.xml");;

