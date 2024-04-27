# Warehouse Automation System API

This repository contains the source code for the warehouse automation system web service. This project was featured in the **TerroByte Hackathon 2024** at Turin Polytechnic University in Tashkent.

## Technologies Used

- [Haskell](https://www.haskell.org/)
- [Servant](https://docs.servant.dev/en/stable/index.html) (API)
- [PostgreSQL](https://www.postgresql.org/) (DB)
- [Docker](https://www.docker.com/) 
- [GitHub Actions](https://docs.github.com/en/actions) (CI/CD)

## Getting Started

First, clone the repository on your local machine:

```sh
$ git clone https://github.com/junkidesu/warehouse-automation-system
```

### Prerequisites

#### Build Tools

To build and run the application locally, ensure that the following are installed:

- [Stack](https://docs.haskellstack.org/en/stable/)
- [Cabal](https://cabal.readthedocs.io/en/stable/)
- [Docker](https://www.docker.com/)

Stack and Cabal can be installed either independently or with the [GHCup](https://www.haskell.org/ghcup/) tool.

#### Services

The application uses PostgreSQL for the database. Thus, a running PostgreSQL server (either local or remote) is required.

#### Environment Variables

See [`.env.sample`](./.env.sample) to see the environment variables that must be set. You can either place them in a `.env` file, or supply them directly to the executable.

### Build and Start Executable

At the root of the repository, run the following:

```sh
$ stack install
$ warehouse-automation-system-exe
```

### Start in Container

You may start the application along with a local PostgreSQL server using Docker Compose.

```sh
$ docker compose -f docker-compose.dev.yml up
```

## Documentation

Swagger documentation of the API is available at https://warehouse-automation-system-main.onrender.com/swagger-ui.