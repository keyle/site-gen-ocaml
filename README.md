### Site generator

This is a static website generator, that includes support for a blog, with vanity urls, keyword tags, RSS, sitemap.xml.

I reimplemented `site-gen-rust` which I wrote in Rust to learn Rust, in OCaml to learn OCaml (which is a rewrite from `site-gen` originally written in Swift). You get the idea.

Note, the Rust and OCaml versions just went further, they're on par feature wise. The `site-gen` Swift version is out of date, I was still figuring out _what_ this thing should be and how it should work.

### How it works

it takes a .settings.json of this format...

```
{
    "workdir": "/Users/name/Documents/Code/website/public",
    "webroot": "https://website.org",
    "template": "./template/template.html",
    "templateindex": "./template/template-index.html",
    "contenttag": "{{%%content%%}}",
    "titletag": "{{%%title%%}}",
    "descriptiontag" : "{{%%description%%}}",
    "keywordstag": "{{%%keywords%%}}"
}
```

The site generator will then 

- walk recursively the `workdir` looking for `.md` (markdown) files, 
- converts them to HTML
- insert them in `template`
- replacing the `contenttag` with the HTML
- update `descriptiontag` with the contents of `<x-desc>` (custom valid HTML5 tag in the markdown)
- same with the `keywordstag` with the contents of `<x-tags>` (hidden)
- same with the `titletag` with the contents of `<x-title>`
- it will do the same with the templateindex if the markdown contains <x-index/> (as an indicator of being the index)

This custom HTML5 tag gymnastic is to avoid having metadata json files around, or breaking the valid markdown format (like Hugo does). In retrospect, I have mixed feelings about it.
  
NOTE: the content will be placed in situe. So if the website places a markdown in `/folder` it will be `/folder/index.html` so that you're in control of the whole website structure and vanity urls.
  
### Blogging
  
The blogging system works pretty much the same, except I use `<x-blog-title>` instead of `<x-title>` to tell the generator that this is a blog post. 

Blog posts will automatically have a `blog` class on the `<body>` to style the blog differently, as needed.

Additionally we parse `sub` for the RSS `pubDate` and to be sorted on the index page.

It will be included in the RSS and linked from the homepage. Note that this last part is pretty much custom to suit my own needs, but could be abstracted out further.

### Running

#### Debug

```
make # or dune exec sitegen
```

#### Release 

```
make build
```

### Benchmarks

Because I'm a nerd I also like to run stats. This compares the different build. Note that both were written pretty naively, I was new to Rust and I am new to OCaml. So, take those with a grain of salt. However the implementation logic is very similar...


#### Rust version

`  Time (mean ± σ):       4.3 ms ±   0.2 ms    [User: 2.1 ms, System: 2.0 ms]`

#### OCaml version

`  Time (mean ± σ):       6.4 ms ±   0.2 ms    [User: 3.5 ms, System: 2.8 ms]`

#### OCaml bytecode version

`  Time (mean ± σ):      21.2 ms ±   0.2 ms    [User: 17.6 ms, System: 3.3 ms]`

#### Method

`hyperfine --warmup 5 ./sitegen`
