# studienarbeit-dash
[![Build Status](https://travis-ci.org/ob-fun-ws17/studienarbeit-dash.svg?branch=master)](https://travis-ci.org/ob-fun-ws17/studienarbeit-dash)
___

Dies ist eine Studienarbeit für das Fach Funktionale Programmierung im WS 17/18.
Dash ist ursprünglich als Backend für ein Dashboard gedacht gewesen. Mangels interessanten Anwendungsfällen 
wurde se zu einem Testprojekt für verschiedene Haskell-Technologien.


## Bauen und Ausführen

Diese Anwendung kann mit stack gebaut aund ausgeführt werden.
Nach dem Clonen wird hierzu im Hauptverzeichnis ausgeführt:
```Bash
stack build
stack exec dash-server
```

Nun läuft die Anwendung lokal und hört auf Port **8080**.
Die Funktionsfähigkeit kann mit `http://localhost:8080/test` überprüft werden.

## Eingesetzte Technologien

Die Anwendung besteht aus einem servant-Webserver und einer SQLite 3 Datenbank. Auf die Datenbank wird zur 
Vermeidung von möglicherweise fehlerhaften SQL-Abfragen mit Hilfe von persist zugegriffen.

Die Generierung des Datenbank Schemas erfolgt mit Hilfe der Haskell-Spracherweiderungen *TamplateHaskell* und *QuasieQuotes*. Diese ermöglichen eine einfache Deklaration des Schemas. Alle nötigen Datentypen und Instanzen in Haskell werden zur Compilezeit generiert.

## WEB-API

Die wesentliche API ist in zwei Teile aufgeteilt: *Todo* und *Task*.

### Todo

Todo stellt die Funktionalitäten für eine Todo-Liste zur Verfühgung.

Beispiel Todo:

```
{
	"todoContext":		"Context", 	// Name des Todo
    "todoStatus":		"Open",		// Status
    "todoCategory":		1,			// ID der Kategorie
    "todoPriority":		"High",		// Priorität
    "todoDeadline":		"2017-11-17",	// Deadline
    "todoDuration":		1			// Dauer
}
```

Die verfügbaren Elemente für Status und Priorität sind in der [Types](https://ob-fun-ws17.github.io/studienarbeit-dash/Types.html) Dokumentation.<br>
Alle relevanten URLs hierzu sind unter `http://localhost:8080/todo` zu erreichen:

- **`GET http://localhost:8080/todo`**<br>
Listet alle vorhandenen Todos.

- **`POST http://localhost:8080/todo/addCategory/?category=newCategory`**<br>
Fügt eine neue Todo-Kategorie hinzu.<br>
Name der Kategorie: "newCategory"<br>
**Antwort**: ID der neu hinzugefügten Kategorie<br>

- **`GET http://localhost:8080/todo/:id`**<br>
Listet das Todo mit der ID auf.<br>

- **`GET http://localhost:8080/todo/add`**<br>
Fügt ein neues Todo hinzu. Erwartet das Todo als Request-Body. (Die benutzte Kategorie-ID muss existieren!)<br>
**Antwort**: ID des neu hinzugefügten Todos<br>

- **`GET http://localhost:8080/todo/remove/:id`**<br>
Entfernt das Todo mit der ID.<br>

- **`GET http://localhost:8080/todo/check`**<br>
Listet alle Todos auf deren Deadline vor dem heutigen Tag liegt.<br>


### Task

Task stellt die vereinfachten Funktionalitäten zum Auflösen eines Abhängigkeitsbauems dar.

Beispiel Task

```
{
  "name":	a", 		// Name der Task
  "dependencies":		// Array mit Abhängigkeiten
  [
	  {
       	"depends":	1,	// ID der Task
        "major":	1,	// Releasenummer
        "minor":	0	// Patchnummer
      }
  ]
}
```


Alle relevanten URLs hierzu sind unter `http://localhost:8080/task` zu erreichen:

- **`GET http://localhost:8080/task/add`**<br>
Fügt eine neue Task hinzu. Erwartet die Task als Request-Body. (Die benutzten Task-IDs der Abhängigkeiten müssen existieren!)<br>
**Antwort**: ID der neu hinzugefügten Task<br>

- **`GET http://localhost:8080/task/sort`**<br>
Liefert die IDs aller Abhängigkeiten zurück, so dass gilt: Wenn n vor n+1 verfügbar ist, sind  sind alle Abhängigkeiten erfüllt.
