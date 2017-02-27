---
title: Programming Workflows with Grammars
url-video: https://www.youtube.com/watch?v=vDjg6XPgjxQ
url-slides: http://bobkonf.de/2015/slides/middelkoop.pptx
authors:
 - Arie Middelkoop
source: http://bobkonf.de/2015/middelkoop.html
conference: BOB2015
tags:
 - Attribute Grammars
libraries:
---

This talk applies compiler techniques to financial consultancy software: We present a workflow framework for the implementation of wizards/assistents tailored to consultancy processes. This type of application requires that the user can revert or refine previous inputs, thereby affecting the structure of the workflow.

Usually, such frameworks model workflows with control flow graphs, which do not capture the influence of data changes. To deal with this problem, we propose a model based on data flow graphs instead, meaning that workflows can be seen as functions in a functionally reactive program.

In our type of application, typically a lot of the inputs are needed at the end of the workflow to generate a report. Consequently, hundreds of values need to be passed around explicitly. The sheer amount of data to be propagated is a challenge that needs to be adressed. This is complicated by the fact that workflows should to be composable (function composition, fold/map, etc.), and thus require context information to be passed around.

Since the composition of workflows forms a tree structure in general, we describe the workflows using a particularly suitable formalism known from compiler construction: higher-order reference attribute grammars. Using this formalism, the propagation of values can be described in an aspect-oriented fashion. Moreover, convenient abstractions are available for particular propagation patterns, similarly to what reader/writer/state monads offer in functional programming. The formalism does not only adress the challenge, but is also of a declarative nature.

We present our ideas at a high level, sketching the implementation of the framework, with the intent of promoting the use of compiler techniques in the development of frameworks.
