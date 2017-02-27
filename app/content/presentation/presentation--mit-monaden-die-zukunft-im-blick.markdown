---
title: Mit Monaden die Zukunft im Blick
url-video: https://www.youtube.com/watch?v=BQzJqdNASyg
url-slides: http://www.joachim-breitner.de/publications/MonadFix_BobKonf2016_2016-02-19.pdf
authors:
 - Joachim Breitner
source: http://bobkonf.de/2016/breitner-monaden.html
conference: BOB2016
tags:
 - Monads
libraries:
---

Man muss keine Monaden kennen, um Haskell zu programmieren, denn es ist nur eine Abstraktionsschicht, also ein Muster, mit dem man seine Programme strukturiert. Als solches aber ein sehr erfolgreiches, und mit der richtigen Monade werden komplizierte Programmieraufgaben plötzlich einfach, übersichtlich und weniger fehlerträchtig.

Eine oft übersehene Variante der Monade wird durch die `MonadFix`-Typklasse beschrieben: Monaden, in denen sich Fixpunkte berechnen kann. Damit kann man, in gewisser Weise, Werte benutzen, die erst später berechnet werden.

In einer Live-Coding-Sitzung zeigt Joachim Breitner, wie er dank einer selbstgestrickten Monaden die Ausgabe eines Binär-Datenformates „schön“ implementiert, wie er dem mit MonadFix den letzten Schliff gibt, und warum das eigentlich überhaupt funktionieren kann.

Dieser Vortrag ist kein Monaden-Tutorial, sondern spricht eher jene an, die ein solches schon einmal bearbeitet haben und nun sehen wollen, wie man dieses Programmiermuster in der Praxis erfolgreich einsetzt. Wer mit Monaden noch nicht gearbeitet hat, kann natürlich entweder schnell mitdenken, oder sich einfach nur vom Ergebnis beeindrucken lassen.
