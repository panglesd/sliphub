# Sliphub

This repository contains:
- The [sliphub](https://sliphub.slipshow.org) source code. Sliphub is a web
  frontend for [slipshow](https://slipshow.org), where you can create
  presentations in the browser, with an editor on the left and a preview on the
  right.
- The Slipshow GUI. This is a Tauri app version of sliphub, for local
  development. The development of the GUI is currently paused, to focus on the
  development of slipshow and the local editor experience, and assess whether
  the Tauri app still makes sense when slipshow has good editor integration and
  easy installation.

<!-- ![image](https://github.com/panglesd/sliphub/assets/34110029/0fa89041-08ba-4a95-aec7-b47664cabd8c) -->

## Hosting

Sliphub is self-hostable. Self-host it!

### Requirements

In order to self-host sliphub, you'll need:
- Linux or Mac. Windows support will be added later.
- OCaml and its package manager, Opam. The former will be installed by the
  latter. Follow the instructions of the [official installation
  guide](https://ocaml.org/install#linux_mac_bsd).
- [PostgreSQL](https://www.postgresql.org/). Follow the instruction of your
  distribution.

### Setup Postgresql

Sliphub will need a role with a password and a database. Create those with
`psql` and `createdb`:

```shell
$ sudo -u postgres psql -c "CREATE ROLE sliphub LOGIN PASSWORD '<password of your choice>';"
$ sudo -u postgres createdb -O sliphub sliphub_db
```

Remember the password!

### Install Sliphub

We suppose `opam` is installed and setup. Install Sliphub in a dedicated switch:

```shell
$ opam switch create sliphub 5.5.0
$ opam pin sliphub git+https://github.com/panglesd/sliphub#main
```

## Run Sliphub

You just need to pass the URL to access the database through an environment variable:

```shell
$ export DATABASE_URL="postgresql://sliphub:<password of your choice>@localhost/sliphub_db"
$ opam exec -- sliphub --port 8080
```

Connect to `localhost:8080`: this is your self-hosted sliphub server!
