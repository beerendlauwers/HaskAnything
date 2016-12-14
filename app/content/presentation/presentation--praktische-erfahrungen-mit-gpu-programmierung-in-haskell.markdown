---
title: Praktische Erfahrungen mit GPU-Programmierung in Haskell
url-video: https://www.youtube.com/watch?v=1kCZzDSnVL0
url-slides: http://bobkonf.de/2015/slides/thielemann.pdf
authors: Henning Thielemann
source: http://bobkonf.de/2015/thielemann.html
conference: BOB2015
tags:
 - GPU Programming
 - DSL
libraries:
 - accelerate
 - Obsidian
 - cuda
---

Man hört in den letzten Jahren zunehmend von modernen massiv parallel rechnenden Grafik-Prozessoren, die sich auch für nicht-grafische Anwendungen einsetzen lassen. Einsatzgebiete sind unter anderem Wetter- und andere physikalische Simulationen, Molekülmodellierung und Entschlüsselung von Passwörtern. Für Haskell gibt es inzwischen mehrere Bibliotheken (‘accelerate’, ‘Obsidian’, ‘Nikola’), die die Rechenleistung der Grafikkoprozessoren auf deklarative Weise nutzbar machen. Das Accelerate-Projekt ist auf die Verarbeitung von mehrdimensionalen Feldern (=Tensoren) zugeschnitten. Auf diese Weise ist es abstrakt genug, um nicht nur Code für Grafikprozessoren sondern auch für Hauptprozessoren und theoretisch auch für programmierbare Logikbausteine (FPGA) zu erzeugen. Für einen Kunden habe ich ein Kalibrierungsverfahren zuerst mit hmatrix/LAPACK entwickelt und es dann auf Accelerate/CUDA zur Ausführung auf Nvidia-Grafikprozessoren übertragen. Auf diese Weise habe ich einen direkten Vergleich zwischen beiden Ansätzen. In meinem Vortrag möchte ich das Problem grob vorstellen und über meine Erfahrungen bei der Lösung berichten.
