glot-www
========


## Overview
glot-www is the website hosted at [glot.io](https://glot.io).
Code are run through [docker-run](https://github.com/glotcode/docker-run)
PostgreSQL is used as the datastore.


## Environment variables
glot-www takes its configuration from environment variables.
All vars need to be set, no default values are provided.


| Variable name                  | Allowed values     | Example                   | Description                                |
|:-------------------------------|:-------------------|:--------------------------|:-------------------------------------------|
| APPROOT                        | url                | https://glot.io           | Base url to where the app is hosted        |
| PORT                           | 1-65535            | 3000                      | Listen port                                |
| PGHOST                         | hostname           | 10.0.0.12                 | Postgresql host                            |
| PGPORT                         | 1-65535            | 5432                      | Postgresql port                            |
| PGUSER                         | string             | glot                      | Postgresql username                        |
| PGPASS                         | string             | secret-password           | Postgresql password                        |
| PGDATABASE                     | string             | glot                      | Postgresql database name                   |
| DOCKER_RUN_BASE_URL            | url                | http://docker-server:8088 | Url to docker-run                          |
| DOCKER_RUN_ACCESS_TOKEN        | string             | some-secret-token         | docker-run access token                    |
| DOCKER_RUN_RESPONSE_TIMEOUT    | integer            | 60                        | Seconds to wait for the response           |
| MAILGUN_DOMAIN                 | string             | glot.io                   | Mailgun domain                             |
| MAILGUN_API_KEY                | string             | key-1234567890            | Mailgun api key                            |
| ANALYTICS_ID                   | string             | secret-id                 | Google analytics id (optional)             |
| DISABLE_ADS                    | boolean            | true                      | Disable ads (optional)                     |


## Compile from source
```bash
git clone git@github.com:glotcode/glot-www.git
cd glot-www
stack build
```

## Develop with yesod-bin
```bash
git clone git@github.com:glotcode/glot-www.git
cd glot-www
stack build yesod-bin
./dev.sh
```
