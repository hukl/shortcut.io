# scio


## DB Setup

Create two databases with the owner "shortcut"

```SQL
CREATE ROLE shortcut WITH LOGIN PASSWORD 'amazingpassword';
CREATE DATABASE shortcut_dev OWNER shortcut;
CREATE DATABASE shortcut_test OWNER shortcut;
```

Then load the schema, which is in priv/db

```sh
make schema_load
```

Lastly, enter the correct credentials in priv/database.config


## Build

```sh
make
```

## Test

```sh
make test
```

## API

All requests should be sent with the `content-type` header set to `application/json`

After logging in, the cookie should be set and subsequent requests should include the cookie.

### Signup

```
# REQUEST

POST /users
{
    username : [string],
    email    : [string],
    password : [string]
}


# RESPONSE

Status: 201
Body: [no body]
```

### Login

```
# REQUEST

POST /sessions
{
    email    : [string],
    password : [string]
}


# RESPONSE

Status: 201
Headers: set-cookie=[session cookie string]
Body: [no body]
```

### Create Shortcut

```
# REQUEST

POST /shortcuts
{
    url         : [string],
    title       : [string],
    description : [string],
    tags        : [ [string], [string], … ]
}


# RESPONSE

Status: 201
Body: [no body]
```

### Get Shortcuts

```
# REQUEST

GET /shortcuts


# RESPONSE

Status: 200
Body: [
        {
            "id"            : 1,
            "url"           : "http://foo.com",
            "title"         : "Best Website ever",
            "description"   : "What more can I say",
            "screenshot_id" : "cc12c780-fa52-11e9-ae38-0023dfdf2726",
            "tags"          : ["foo", "bar", "baz"],
            "created_at"    : 1572360698,
            "updated_at"    : 1572360698
        },
        { … }
    ]
```
