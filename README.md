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

## License

This work is licensed under the Creative Commons Attribution-NonCommercial 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by-nc/4.0/.
