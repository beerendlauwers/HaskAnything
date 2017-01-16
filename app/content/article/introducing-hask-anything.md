---
title: "Introducing: Hask Anything!"
date: 2016-08-05
permission-file: permissions/article/beerend_lauwers
url: http://beerendlauwers.be/posts/2016-08-05-introducing-hask-anything.html
authors: 
 - "Beerend Lauwers"
type: "blog post"
tags: hakyll
---

*([As originally posted on Reddit](https://www.reddit.com/r/haskell/comments/4wb0n5/introducing_hask_anything/).)*

I am proud to announce the alpha^alpha^alpha release of **[Hask Anything](http://haskanything.com/)**, a website aimed at collecting and organizing the collective knowledge of the Haskell community: papers, blog posts, code snippets, gists, StackOverflow answers, Reddit threads, presentations, videos, tutorials and so on.

It's based on Hakyll and hosted on Github, and my goal is to have it be entirely community-driven.

## What can the community do?

Everything, really! We need people to:

### Test the site functionality
I haven't even tested this thing on mobile browsers yet. There's probably a whole bunch of problems still lurking in the website content submission forms.

### Add content
Yes, you can submit content straight from the website! For example: http://haskanything.com/ui/submit/presentation.html

You can also submit content [via Github](https://github.com/beerendlauwers/HaskAnything/) itself, of course.

### Contribute code

The [Github issue list](https://github.com/beerendlauwers/HaskAnything/issues) has bunch of issues tagged with `newcomer`. Be sure to have a look at the site ["documentation"](http://haskanything.com/documentation.html).

## Q&A

### Q: Holy *crap*, this thing looks hacked together.

**A:** First off, that's not a question. Secondly, yes. It is very much hacked together.
I actually started work on it more than a year ago. Seeing other knowledge repositories pop up (shout-out to [haskell-lang's documentation](https://haskell-lang.org/documentation) and [School of Haskell](https://www.schoolofhaskell.com/)) kept rekindling the fire, and real life kept stomping it out.

I was never going to get this site to the standard that I wanted to uphold at this speed, so I made a decision: even in this alpha state, I would release it and build it out together with the Haskell community.

### Q: So, what *exactly* do you expect this site to become?

**A:**  The main problem I see in the Haskell community is knowledge *discoverability*.
There are already excellent sources of knowledge: the Haskell wiki, excellent blogs, conference presentations on Youtube, Reddit and StackOverflow discussions... But we have no way of *traversing* that knowledge, no way of *filtering* it.

That's what Hask Anything! is about: collecting and organizing the collective knowledge of the Haskell community.

### Q: Can't I just post a tutorial directly on here?

**A:** Yes. I mean, not yet because there's no tutorial category right now, but that's on the to-do list.

But yes, becoming a source of knowledge is also on the roadmap.

### Q: Okay, so what can I do?

**A:** As I mentioned before, you can add content and contribute code. I've seeded the [Github issue list](https://github.com/beerendlauwers/HaskAnything/issues) with a bunch of stuff that people can take on. There are a few relatively simple ones, which I have tagged with `newcomer`. Issues tagged with `Priority 1` are blockers for a bunch of other issues or just very important aspects of the site that still need to be implemented.

### Q: How does the online content contribution work?

**A:** You select a category, fill in the forms (or have some of them automatically filled in via an API lookup) and submit it as a pull request to Github. Oh, you need to log in via OAuth to do that first, of course.

Then, the pull request is reviewed and merged by the review committee (Currently, me). We need to do manual reviews to:

1. prevent malicious stuff to be introduced, and
2. ensure that we have the approval of authors to replicate their work on the site (if applicable).

### Q: Where did you get the name from?

**A:** /u/dagit had some threads with the same title: [https://www.reddit.com/r/haskell/comments/3fnvsg/hask_anything_the_thread_where_you_ask_and_we_try/](https://www.reddit.com/r/haskell/comments/3fnvsg/hask_anything_the_thread_where_you_ask_and_we_try/)
