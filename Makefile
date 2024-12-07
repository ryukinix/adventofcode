GORUN = go run .
LISPRUN = sbcl --noinform --load $(HOME)/.sbclrc --script main.lisp
# For scala2.11 with java>=11, it needs the following arguments
# SCALARUN = scala -nobootcp -nc  Main.scala
SCALARUN = scala Main.scala
PYTHONRUN = python main.py


benchmark := no
lang := scala
year := 2024


ifeq ($(lang), go)
	COMMAND = $(GORUN)
else ifeq ($(lang), lisp)
	COMMAND = $(LISPRUN)
else ifeq ($(lang), scala)
	COMMAND = $(SCALARUN)
else ifeq ($(lang), python)
	COMMAND = $(PYTHONRUN)
else
	printf "language '$(lang)' not supported\n"; exit 1
endif

ifeq ($(benchmark), yes)
	COMMAND := time $(COMMAND)
else ifeq ($(benchmark), time)
	COMMAND := time $(COMMAND)
else ifeq ($(benchmark), hyperfine)
	COMMAND := hyperfine --warmup 1 "$(COMMAND)"
endif


day%:
	@echo "[$(lang)] Solution for $@ challenge: "
	@cd $(year)/$@; $(COMMAND)

check:
	go test -v ./...

build:
	go build -v ./...
