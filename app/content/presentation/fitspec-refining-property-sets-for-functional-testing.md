---
title: "FitSpec: Refining Property Sets for Functional Testing"
url-video: "https://www.youtube.com/watch?v=1oQ1TulCdP4"
url-slides: "about:blank"
authors:
 - Rudy Braquehais
 - Colin Runciman
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-fitspec-refining-property-sets-for-functional-testing
tags:
 - Property-based testing
 - Testing
libraries:
 - fitspec
---

This paper presents FitSpec, a tool providing automated assistance in the task of refining sets of test properties for Haskell functions. FitSpec tests mutant variations of functions under test against a given property set, recording any surviving mutants that pass all tests. The number of surviving
mutants and any smallest survivor are presented to the user. A surviving mutant indicates incompleteness of the property set, prompting the user to amend a property or to add a new one, making the property set stronger. Based on the same test results, FitSpec also provides conjectures in the form of equivalences and implications between property subsets. These conjectures help the user to identify minimal core subsets of properties and so to reduce the cost of future property-based testing.
