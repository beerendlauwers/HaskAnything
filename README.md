[![CircleCI](https://circleci.com/gh/beerendlauwers/HaskAnything/tree/master.svg?style=shield&circle-token=f03b1c993138062b403decf7d653ddf6c84da754)](https://circleci.com/gh/beerendlauwers/HaskAnything/tree/master) [![Appveyor](https://ci.appveyor.com/api/projects/status/github/beerendlauwers/haskanything?svg=true)](https://ci.appveyor.com/project/beerendlauwers/haskanything)

### [Hask Anything!](http://haskanything.com/) is a website aimed at collecting and organizing the collective knowledge of the Haskell community: papers, blog posts, code snippets, gists, StackOverflow answers, Reddit threads, presentations, videos, tutorials and so on.

## Contributing content

You can contribute content in two ways:

1. Via the website itself:
   * Go to http://haskanything.com/.
   * On the top right of the page, click **"You're not logged in."** and select the option **"Login"**. You'll be redirected to the Github authorization page.
   * In the top navigation bar, select an option from the **"Add a new.."** dropdown.
   * Go through the form steps.
   * Click on the **"Generate a preview of the generated file"** button.
   * Finally, click on the **"Submit a pull request"** button.
   
2. Via git. You can just clone the project, copy some existing content, modify it and create a pull request. Useful for adding content in bulk!

## Contributing code

There is more extensive documentation over at http://haskanything.com/documentation.html.
Here's the short version:

We use [Hakyll](https://jaspervdj.be/hakyll/) as a base.
Hakyll is a static site generator written in Haskell, and it's *great*.

The project uses [Stack](https://docs.haskellstack.org/en/stable/README/), so it *should* just be:

1. `stack install`, which builds `hask-anything-exe`
2. In `/app`, run `hask-anything-exe build`, or `site-anything-exe watch` to serve the site locally. 

If it's not, please open an issue over at [Github](https://github.com/beerendlauwers/HaskAnything/issues)!

## Troubleshooting

**Problem:** The CI system gives the following error message: `Hakyll.Web.readPandocWith: I don't know how to read a file of the type Binary for: <file name>`

**Solution:** The file name isn't valid. Does it have the right extension? For Markdown files (which is what most content it) use `.md`.

## Technical info

This Github repository serves as both the code repository that generates the website (branch `master`), as well as the host for the website itself (branch `gh-pages`).

**Note:** Pushes are done automatically to `gh-pages` by CircleCI, which overwrites any changes, so never offer pull requests for that branch.

### Sponsorship

![BrowserStack logo](https://raw.githubusercontent.com/beerendlauwers/HaskAnything/master/app/images/browserstack-badge.png)

[BrowserStack](https://www.browserstack.com/) provides me with a free account because of the [open-source nature of the project](https://www.browserstack.com/pricing).
