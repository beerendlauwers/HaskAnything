CNAME := haskanything.com
REPO := git@github.com:beerendlauwers/HaskAnything.git

EXE := /home/ubuntu/.local/bin/hask-anything-exe

all:	build
	@true

build:	${EXE}
	ls
	cd app
	ls
	${EXE} build

# Deploy _site to Github Pages
deploy:
	cd /home/ubuntu/
	ls
	echo ${CNAME} > _site/CNAME
	rm -rf _site/.git
	cd _site && git init && git add .
	cd _site && git config user.email "nobody@circleci.com"
	cd _site && git config user.name CircleCI
	cd _site && git commit -m "Generated on `date`"
	cd _site && git remote add origin ${REPO}
	cd _site && git push -f origin master:gh-pages