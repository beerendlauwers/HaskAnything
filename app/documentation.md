# Hask Anything App Documentation

This is an informal description of the system that generates the Hask Anything website.

Hask Anything is a statically generated site that aims at being a knowledge repository for anything Haskell: papers, presentations, Reddit threads, StackOverflow posts, etc.
You can submit new content from the website itself after logging in with your Github account.
Submitting content generates a pull request in the HaskAnything Github repository, where further discussion can take place and where the PR can be manually approved.

## Overview

### Hakyll

We use [Hakyll](https://jaspervdj.be/hakyll/) as a base.
Hakyll is a static site generator written in Haskell, and it's *great*.

### Client-side code

Just vanilla javascript with judicious use of jQuery. 
We could definitely use someone to convert this to PureScript or something.
For communication with Github, we use [github.js](https://github.com/michael/github).

### OAuth

To be able to use the content submission forms on the website, we need to authenticate with Github (needs permission "Access public repositories").
Currently, we use a free "bootstrap" account on [oauth.io](https://oauth.io/home).

A good idea is to replace this with the open-source version of oauth.io, namely [oauthd](https://github.com/oauth-io/oauthd).
Just run it on a DigitalOcean droplet or something - for a single domain, the bandwidth and cpu time should be fine.

### Buzzwords

* We use Bootstrap for a responsive layout, and vanilla CSS and JS. 
* Both the CSS and JS should be rewritten to use SASS and PureScript or something.
* The current JavaScript uses jQuery and [ramdaJS](http://ramdajs.com/) in some places.
* We use [handlebars.js](http://handlebarsjs.com/) for generating file contents for the content submission forms.
* There's some leftover code of a React attempt, couldn't find a good use case for it.

## Architectural[^architecture] overview

In the `app` directory, we have everything we need to generate the website.

### Content

First and foremost is the **content**.
In the `content` directory, each directory is a "category" (`presentation`,`package`,`reddit-post`,...).
Content of a particular category may be given a special place in the website, like `how-do-i` content.

In general, though, content is just a Markdown file with a bunch of metadata.

### Submission UI

#### HTML

This lives in the `ui` directory.
There's two subdirectories: `elements` and `submit`.

The first one contains HTML snippets for a particular form element.
In many cases, a form element can be reused across several submission interfaces.
For example, here is the HTML snippet for a "title" form element:

```html
<div class="row col-sm-12 web-submit-element">
    <div class="col-sm-3">
        <label for="title">Title:</label>
    </div>
   <div class="col-sm-9">
        <input type="text" id="title"/>
    </div>
</div>
```

In the `submit` directory, we glue together these HTML snippets to generate a single submission form, which has three tabs: *Content name*, *Metadata* and *Submit*.
There's some common HTML in each file that could also be factored out.
The unique part is usually in the first tab.
As another example, here is what the code for the first tab for the presentation submission form looks like:

```
<div class="tab-pane" id="tab1">
    $partial("ui/elements/title-presentation.html")$
    $partial("ui/elements/url-video-presentation.html")$
    $partial("ui/elements/url-slides-presentation.html")$
    $partial("ui/elements/description-presentation.html")$
    $partial("ui/elements/authors-presentation.html")$
    $partial("ui/elements/conference-presentation.html")$
</div>
```

#### Submission logic

To perform a submission, we also need some JavaScript that can do the following:

1. Get the values from the form fields.
2. Put them in a [Handlebars](http://handlebarsjs.com/) template.
3. Select a value and use it to generate a unique name for the file.
4. Submit the file to Github.

Numbers **1** and **2** are done in `app/js/generate-file-preview.js`, by `generateFilePreview()`.
`displayFilePreview()` calls ´generateFilePreview()`, and is used to put it in the preview textarea.

Imagine number **3** as a type class: `(IsContentType a) => HasFileTitle a`, with a type class function `generateFileTitle :: a -> String`.

The "type class instances", then, can be found in `app/js`: `github-lookup.js`, `paper-lookup.js`, `presentation-lookup.js`, and so on.
Some of these files can also contain another "type class instance" for the data scraping logic, which is described in the next section.

For number **4**, we have `app/js/pull-request.js`.
The function used as an entry point is `collectDataAndSubmitPullRequest()`.
The actual work is done in `submitHaskAnythingPullRequest()`, which really, *really*, ***really*** needs a rewrite to use Promises.

#### Data scraping logic

In some cases, the ability to prefill some of the form fields with data is necessary, as well.

In `app/js/ui/submit`, we have a JavaScript "class" `Lookup` which handles communication with a URI and data extraction from the response.
Built with "dependency injection", the fancy name for higher-order functions.
Just think of it as another type class `HasLookup a` that has six functions:

```Haskell
class HasLookup a where

    -- From a URL, extract the parts (UrlInfo a) we need to inject into an API call.
    urlInspector :: String -> (UrlInfo a) 
    
    -- With the parts, construct a valid API call and executes it.
    urlConstructor :: (UrlInfo a) -> IO JSON
    
    -- Checks if the received JSON is valid.
    messageValidator :: JSON -> Bool
    
    -- Extract the useful information from the JSON.
    messageAccessor :: JSON -> b
    
    -- Display the message somewhere (usually a textarea).
    messageRenderer :: b -> IO ()
    
    -- Function that calls the above functions. Has a default implementation.
    urlCaller :: String -> IO ()
```

The "instances" for this type class can also be found in `app/js`: `github-lookup.js`, etc.
The `Lookup` class is also instantiated in these files, usually at the bottom:

```JavaScript
var githubLookup = new Lookup( inspectURL, constructURL, messageIsValid, accessMessage, undefined, renderMessage );

function lookupGithubRepository() {
    githubLookup.lookup();
}
```

These lookup functions are usually hooked up to a button in the submission form HTML.
For example, here's the HTML code for the button for a Github repository:

```HTML
<button type="button" class="preview" onclick="lookupGithubRepository();return false;">Fetch repository details</button>
```

### Templates

Templates. *Boy*, do we have *templates*.
There's templates for a piece of content itself, templates for the categories, Handlebars templates, templates for tags, and a whole bunch more.
They all live in `app/templates`.

#### Categories

In the `categories` subdirectory, there's one template for each category.
These just contain a simple description of what the category is.
For example:

```HTML
A <i>presentation</i> is a video recording of a presentation along with a slide deck.

<a href="/ui/submit/presentation.html">Submit a new presentation</a>
```

It's used on the [category overview page](/categories/presentation.html).

#### Content

In the `content` subdirectory, we have the templates for a single piece of content.
For example, here's the template for a "snippet":

```HTML
<div class="col-md-12 col-sm-12 intro">
        <h1>$title$</h1>
        <h2 class="authors">
        $if(author)$
            By $author$
        $endif$
        </h2>
</div>

<div class="col-md-12 col-sm-12">
        <h1>Contents</h1>
        $body$
</div>

<div class="col-md-12 col-sm-12">
        <h1>Tagged with</h1>
        $tags$ $category$ $libraries$
</div>
```

#### Tags

Some experiments with rendering tags.
`categorylist.html` and `navbarlist.html` are used in `app/templates/default.html`, with the first being a list of categories and the latter a list of facets.

Facets are pretty much just filters of the content: you can view all content of the category "presentation", or view all content with the tag "io", or view all content related to the library "pandoc".

The tagging system is still a work in progress, but the general gist is the following:

* Category: automatically assigned based on which subdirectory the content is in.
* Tags: free-input tags.
* Libraries: I didn't want to put libraries under tags as well, because there might be tutorials for older versions of a library (it'd be horrible to get tags like `hakyll-4.8.0.1`, `hakyll-4.8.3.2`), and it would be nice to be able to list all related to a particular library under the library page.

#### Handlebars

These templates are used by the handlebars.js library to generate the contents of a file that is pushed to Github when submitting via the website.

#### Various

There are a few more templates in `app/templates` itself:

* categories.html, categories-overview.html, libraries.html and tags.html are currently unused.
* content.html is a snippet that is included on a content page, adding a link that links to the file in edit mode on Github.
* default.html is the main template.
* submit.html is a wrapper template used by the submission UI. It includes some JavaScript and CSS.

## Code

Code is in two places: `app/site.hs` and the `src/HaskAnything` folder.
In general, `site.hs` purely does the data transformation of the source files to the final website.
That main function is 160 lines, so pretty small.

`src/HaskAnything` contains support functions that are specific for HaskAnything functionality.
Some stuff could probably be generalized and put in [hakyll-extra](https://github.com/beerendlauwers/hakyll-extra).

## Getting it up and running

The project uses [Stack](https://docs.haskellstack.org/en/stable/README/), so it *should* just be:

1. `stack build`
2. Copying the built executable into the `app` folder. (Help on doing this automatically is greatly appreciated)
3. In `/app`, run `site build`, or `site watch`.

If it's not, please open an issue over at [Github](https://github.com/beerendlauwers/HaskAnything/issues)!

[^architecture]: **Disclaimer:** May or may not resemble a proper architecture.