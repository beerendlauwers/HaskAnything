CNAME := haskanything.com
REPO := git@github.com:beerendlauwers/HaskAnything.git

EXE := /home/ubuntu/.local/bin/hask-anything-exe

all:	build
	@true

build:	${EXE}
	cd app && ${EXE} build

# Deploy _site to Github Pages
deploy:
	cd app && echo ${CNAME} > _site/CNAME
	cd app && rm -rf _site/.git
	cd app/_site && touch .nojekyll
	cd app/_site && git init && git add .
	cd app/_site && git config user.email "nobody@circleci.com"
	cd app/_site && git config user.name CircleCI
	cd app/_site && git commit -m "Generated on `date`"
	cd app/_site && git remote add origin ${REPO}
	cd app/_site && git push -f origin master:gh-pages
