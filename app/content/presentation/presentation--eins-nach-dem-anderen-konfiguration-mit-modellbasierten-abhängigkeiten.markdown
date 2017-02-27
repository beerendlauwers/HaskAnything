---
title: "Eins nach dem anderen: Konfiguration mit modellbasierten Abhängigkeiten"
url-video: https://www.youtube.com/watch?v=UflO7YBdhYA
url-slides: http://bobkonf.de/2015/slides/greif.pdftx
authors:
 - Gabor Greif
source: http://bobkonf.de/2015/greif.html
conference: BOB2015
tags:
 - DSL
libraries:
 - gdiff
---

Das Konfigurieren von eingebetteten Devices ist nach wie vor ein fehlerträchtiges Gebiet. Missachtung der elementaren Einstellungsschritte, wie z.B. von der Hardware diktiert, kann zu transientem (oder schlimmerem) Fehlverhalten führen. Ein Beispiel:

Haben wir einen Wecker, den wir über CLI konfigurieren wollen, dann wäre so ein Kommando denkbar:

    $ set-alarm -time Now -active Off

Die “naive” (d.h. links-nach-rechts) Abarbeitung der zwei Teilaktionen kann dazu führen dass der Wecker (transient) losgeht, sofern er vorher

auf eine andere Zeit gestellt war (und deshalb gerade ruhig)
zugleich der Alarm aktiv war
Das ist offenbar gegen den Willen des Users, der den Wecker ausmachen will. Die Reihenfolge der Teilaktionen ist also kontext-sensitiv.

Wir schlagen einen neuartigen Ansatz vor, bei dem die sukzessiven Veränderungen des Konfigurationsbaums mit einem Datentyp-generischen Algorithmus in Deltas zerlegt und in atomare Aktionen aufgebrochen werden. Die resultierenden Aktionen werden jedoch nicht sofort ausgeführt, sondern dienen als Input zu einer Prioritätsfindung, deren (stark typisierte) Regeln sich aus domänenspezifischen Bedingungen ableiten lassen. Die Kausalitäten erlauben es uns sogar Rückschlüsse auf eventuell nebenläufige Ausführung auf Vielkern-CPUs zu ziehen. Wir verwenden die Haskell “gdiff” Bibliothek und legen uns bewusst erst sehr spät fest wie die Aktionen realisiert werden. Das Typsystem garantiert die korrekte Reihenfolge der endgültigen Ausführung.

Der Vortrag beinhaltet Ergebnisse aus der laufenden Bachelorarbeit von Philip Ottinger und bisher unveröffentlichte Inhalte für das vom BMBF geförderte Projekt SASER. Gabor Greif und Philip Ottinger sind bei der Alcatel-Lucent Deutschland AG beschäftigt.
