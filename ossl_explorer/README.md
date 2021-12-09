# Ossl Explorer

OSSL explorer service

## Useful links
* [Link](http://explorer.soilspectroscopy.org/)
* [Documentation](https://soilspectroscopy.github.io/ossl-manual/database.html#soillab-table)

## Deploy Docker
Build the docker image
```
docker build . -t rshiny_explorer
```

Run the docker - locally
```
docker run -d --name rshiny_explorer -p 3838:3838 rshiny_explorer
```