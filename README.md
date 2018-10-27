[![Build Status](https://travis-ci.org/DukkhaLess/DukkhaLessServer.svg?branch=master)](https://travis-ci.org/DukkhaLess/DukkhaLessServer)
# DukkhaLessServer
The backend server for the DukkhaLess service.

## Setting up your development environment

### Linux (And I think mac too)
- Install haskell-platform
  - [Download and instructions here](https://www.haskell.org/platform/#linux-generic)
  - Don't forget to perform additional instructions on that page, such as the PIE flag for ubuntu users.
- Update your local package database of haskell packages
  - `cabal update`
- Add an environment variable to your `~/.profile`
  - `export POSTGRES_PASSWORD=secret`
- Source your `~/.profile` to add the variable to the current terminal window
  - `source ~/.profile`
- Start the docker container
  - `sudo docker run --name DukkhalessDB -e POSTGRES_PASSWORD=$POSTGRES_PASSWORD -p 5432:5432 --restart unless-stopped -d postgres`
- It should now be possible to run the test suite.
  - `stack test`
- Set up an application user for the server.
  ```sql
  psql -h localhost -p 5432 -U postgres -W
  CREATE DATABASE dukkhaless;
  CREATE USER dukkhaless WITH ENCRYPTED PASSWORD secret;
  GRANT ALL PRIVILEGES ON DATABASE dukkhaless TO dukkhaless;
  ALTER DATABASE dukkhaless OWNER TO dukkhaless;
  ```
