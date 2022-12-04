GORUN = go run .
LISPRUN = sbcl --script main.lisp
lang := go
year := 2022
ifeq ($(lang), go)
	COMMAND = $(GORUN)
else ifeq ($(lang), lisp)
	COMMAND = $(LISPRUN)
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
