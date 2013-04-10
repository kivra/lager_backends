lager_backends (v0.1.0)
Custom backends for lager:
* lager_backend_email
* lager_backend_rabbitmq

lager_backends is released under the terms of the [MIT](http://en.wikipedia.org/wiki/MIT_License) license

copyright 2012-2014 Kivra

## tl;dr
### Example app.config:
```erlang
{lager, [{ handlers
         , [ { lager_backend_email
             , [warning, "machine@foo", "alarms@foo"] }
           , { lager_backend_rabbitmq
             , [ info
               , [{"mq1@foo", 5672}, {"mq2@foo", 5672}]
               , "username"
               , "password"
               , "exchange"
               , "routing key"
               ]}
            ]}
        ]}.
```
