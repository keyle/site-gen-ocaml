
type post = {
    path: string;
    file: string;
    mutable markdown: string;
    mutable html: string;
    mutable title: string;
    mutable is_blog: bool;
    mutable url: string;
    mutable pub_date: string;
    mutable description: string;
    mutable tags: string list;
} 
(* [@@deriving show] note overriden manually below *)

let show_post { title; is_blog; path; file; url; _ } = 
    Printf.sprintf "post { title: %s; is_blog: %b; path: %s; file: %s; \nvanity: %s}" title is_blog path file url

let default_post = {
    path = "";
    file = "";
    markdown = "";
    html = "";
    title = "";
    is_blog = false;
    url = "";
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