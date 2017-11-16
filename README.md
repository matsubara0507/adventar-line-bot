# adventar-line-bot
LINE Bot to notify update of ADVENTAR with Haskell

## Requires

- selenium with chrom webdriver

## Usage

run selenium with chrome webdriver.

e.g.

```
$ java -jar ./selenium-server-standalone-*.jar
```

and, run Bot

```
$ stack exec -- adventar-line-bot "https://adven tar.org/calendars/0000" entry.json
```
