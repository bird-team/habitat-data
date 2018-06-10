# Main instructions
## build data
all: data

# Define variables
ifdef ComSpec
	RM=del /F /Q
	RMDIR=rmdir
	PATHSEP2=\\
	MV=MOVE
else
	RM=rm -f
	RMDIR=rm -rf
	PATHSEP2=/
	MV=mv
endif

# Individual commands
## clean data
clean:
	@$(RM) exports/habitat_data.shp
	@$(RM) exports/habitat_data.shx
	@$(RM) exports/habitat_data.dbf
	@$(RM) exports/habitat_data.prj

## make data
data:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker exec bba sh -c "Rscript code/make_habitat_data.R" \
	&& docker cp bba:/tmp/exports/habitat_data.shp exports \
	&& docker cp bba:/tmp/exports/habitat_data.shx exports \
	&& docker cp bba:/tmp/exports/habitat_data.prj exports \
	&& docker cp bba:/tmp/exports/habitat_data.dbf exports || true
	@docker stop -t 1 bba || true && docker rm bba || true

# docker container commands
## pull image
pull_image:
	@docker pull 'brisbanebirdteam/build-env:latest'

## remove image
rm_image:
	@docker image rm 'brisbanebirdteam/build-env:latest'

## start container
start_container:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest'

## kill container
stop_container:
	@docker stop -t 1 bba || true && docker rm bba || true

# PHONY
.PHONY: data clean pull_image rm_image start_container stop_container
