GORUN = go run .
LISPRUN = sbcl --noinform --load $(HOME)/.sbclrc --script main.lisp
SCALARUN = scala -nobootcp -nc  Main.scala

lang := scala
year := 2024

ifeq ($(lang), go)
	COMMAND = $(GORUN)
else ifeq ($(lang), lisp)
	COMMAND = $(LISPRUN)
else ifeq ($(lang), scala)
	COMMAND = $(SCALARUN)
else
	COMMAND = printf "language '$(lang)' not supported\n"; exit 1
endif

day%:
	@echo "[$(lang)] Solution for $@ challenge: "
	@cd $(year)/$@; $(COMMAND)

check:
	go test -v ./...

build:
	go build -v ./...
