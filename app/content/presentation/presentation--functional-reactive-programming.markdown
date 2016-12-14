---
title: Functional Reactive Programming
url-video: https://www.youtube.com/watch?v=-wI-TEKbmxc
url-slides: http://bobkonf.de/2016/slides/apfelmus.pdf
authors: Heinrich Apfelmus
source: http://bobkonf.de/2016/apfelmus.html
conference: BOB2016
tags:
 - Functional Reactive Programming (FRP)
libraries:
 - reactive-banana
 - threepenny-gui
---

In 1968, in a presentation that would later become known as the “the mother of all demos”, computer scientist Douglas Engelbart and his team started the personal computer revolution by showcasing the first graphical user interface, which included key innovations such as the mouse, hypertext links, videoconferencing and shared-screen editing.

But building such a groundbreaking system also required building new programming languages. Engelbart’s system was implemented using custom high-level languages that were developed together with the system itself, and the graphical user interface has remained a source for language innovation ever since. For instance, the next milestone in interactive computing, the Xerox Alto system, introduced the first object-oriented language, Smalltalk, in 1973.

Today, programming graphical user interfaces with object-oriented languages has become mainstream, but is still suprisingly complex. I think it is time for a new step in the evolution of programming paradigms. In recent years, the idea of functional reactive programming (FRP) has shown great promise for programming interactive systems in an elegant manner.

In this talk, I will give a short introduction to functional reactive programming (FRP), using the Haskell programming language. First, I will explain the key idea of representing event streams, like mouse clicks, and program state, like the mouse position, in terms of infinite lists and functions – and why this simple idea actually leads to better code. Then, we will look at a practical implementation of this idea and explore several examples using my [reactive-banana](https://wiki.haskell.org/Reactive-banana) FRP library and the [threepenny-gui](https://wiki.haskell.org/Threepenny-gui) library.

We probably won’t have time to cover FRP systems for other languages, like sodium for Java, but I will point out why libraries from the growing “React” family are strictly less capable than a properly designed FRP system: They lack a deterministic “merge” operation.
