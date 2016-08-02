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

1. `stack build`
2. Copying the built executable into the `app` folder. (Help on doing this automatically is greatly appreciated)
3. In `/app`, run `site build`, or `site watch`.

If it's not, please open an issue over at [Github](https://github.com/beerendlauwers/HaskAnything/issues)!

## Technical info

This Github repository serves as both the code repository that generates the website (branch `master`), as well as the host for the website itself (branch `gh-pages`).

**Note:** Pushes are done automatically to `gh-pages` by CircleCI, which overwrites any changes, so never offer pull requests for that branch.
