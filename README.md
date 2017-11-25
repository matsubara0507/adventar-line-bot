# adventar-line-bot
LINE Bot to notify update of ADVENTAR with Haskell

## Requires

- selenium with chrom webdriver
- Google Cloud Datastore and Google Cloud Functions

## Usage

### Collect LINE ID

use Cloud Functions, and store for Cloud Datastore.

deploy js script for cloud functions.

```
$ cd cloud_function
$ gcloud beta functions deploy adventar-bot --stage-bucket hoge-dump --trigger-http --entry-point handler
```

### Post Message for LINE

use GCE.

run selenium with chrome webdriver.

e.g.

```
$ java -jar ./selenium-server-standalone-*.jar
```

and, run Bot

```
$ LINE_TOKEN="XXX" PROJECT_ID="YYY" WD_HOST="localhost" WD_PORT="4444" \
  stack exec -- adventar-line-bot "https://adven tar.org/calendars/0000" config/entry.json "KIND_OF_CLOUD_DATASTORE"
```

### Use docker-compose

```
$ LINE_TOKEN="XXX" PROJECT_ID="YYY" DATASTORE_KIND="Hoge" HTML_URL="https://adven tar.org/calendars/0000" TIME=10 docker-compose run bot
```

### Use cron

```
$ sudo su
# cd
# git clone https://github.com/matsubara0507/adventar-line-bot.git
```

write with `crontab -e`

```
MAILTO=""
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

HTML_URL="https://adventar.org/calendars/0000"
LINE_TOKEN="XXX"
PROJECT_ID="YYY"
DATASTORE_KIND="Hoge"
TIME=10

0 * * * * (docker-compose -f /root/adventar-line-bot/docker-compose.yml run --rm bot ; docker-compose -f /root/adventar-line-bot/docker-compose.yml stop) >> /root/test.log 2>> /root/error.log
```

and, re-write docker-compose.yml

```yaml
volumes:
    - /root/adventar-line-bot/config:/config
```
