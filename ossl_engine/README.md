# OSSL Engine

OSSL engine service

## Useful links
* [Link](http://engine.soilspectroscopy.org/)
* [Documentation](https://soilspectroscopy.github.io/ossl-manual/database.html#soillab-table)

## Deploy Docker
Build the docker image
```
docker build . -t rshiny_engine
```

Run the docker - locally
```
docker run -d --name rshiny_engine -p 3838:3838 rshiny_engine
```