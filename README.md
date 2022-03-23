# CSE 131 Spring 2021

Public course materials for [UCSD CSE 131](https://ucsd-cse131.github.io/sp21) 

## Install

You too, can build this webpage locally, like so:

```bash
git clone https://github.com/ucsd-cse131/sp21.git
cd sp21
make
```

The website will live in `_site/`.

## Customize

By editing the parameters in `siteCtx` in `Site.hs`

## View

You can view it by running

```bash
make server
```

## Update

Either do

```bash
make upload
```

or, if you prefer

```bash
make
cp -r _site/* docs/
git commit -a -m "update webpage"
git push origin master
```
## New Class Checklist

- [x] site.hs
- [x] index.md
- [x] links.md
- [x] contact.md
- [x] lectures.md
- [-] calendar.md
- [-] groups
- [-] seating chart

- [x] grades.md
- [ ] assignments.md
- [ ] vscode issues (https://piazza.com/class/kjivoxdgfuc1w2?cid=128)

- [ ] CANVAS setup
- [ ] ASSIGNMENT setup on ieng

## ieng6 Setup

1. Set the `stack-root`

```
stack setup --stack-root=/software/CSE/cse130/.stack
```

2. Create a shell script

```
cat > fixpaths.sh

cd ~/../public/bin && chmod -R a+rx *
cd /software/CSE/cse130/.stack && chmod -R a+rx *
```

3. For each assignment,

	- `git clone` it to download assignment as instructor
	- `stack test` it to get the relevant libs added to the stack-path
	- `./fixpaths.sh` to allow everyone else to read the libraries

4. For each assignment,
	- login as student to make sure that you can `git clone` and then run `stack test`


## Credits

This theme is a fork of [CleanMagicMedium-Jekyll](https://github.com/SpaceG/CleanMagicMedium-Jekyll) 
originally published by Lucas Gatsas.
