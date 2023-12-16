GORUN = go run .
LISPRUN = sbcl --load $(HOME)/.sbclrc --script main.lisp
lang := lisp
year := 2023
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
