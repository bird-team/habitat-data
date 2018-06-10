# Habitat data

This repository contains raw data and code used to derive the spatial habitat data used in the Atlas of the Birds of Brisbane (https://brisbanebirds.com).

## Usage

To create the habitat data, clone this repository and execute the command `make all`. The spatial data will be output into the _exports_ directory. Once the data has been created, it can be copied into the _data_ directory in the atlas repository (https://github.com/bird-team/brisbane-bird-atlas). To customize data processing, please modify the _data/classification/bvg5m.csv_ file to specify which vegetation groups should be assigned to which habitat classes.
