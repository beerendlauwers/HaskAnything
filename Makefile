CNAME := haskanything.com
REPO := git@github.com:beerendlauwers/HaskAnything.git

EXE := dist/build/site/site

all:	build
	@true

${EXE}:	site.hs
	cabal build
	${EXE} clean

build:	${EXE}
	${EXE} build

clean:
	${EXE} clean

run:	build
	${EXE} watch

# Deploy _site to Github Pages
deploy:
	echo ${CNAME} > _site/CNAME
	rm -rf _site/.git
	cd _site && git init && git add .
	cd _site && git config user.email "nobody@circleci.com"
	cd _site && git config user.name CircleCI
	cd _site && git commit -m "Generated on `date`"
	cd _site && git remote add origin ${REPO}
	cd _site && git push -f origin master:gh-pages