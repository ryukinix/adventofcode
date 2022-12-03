GORUN = go run
YEAR := 2022

day%:
	@cd $(YEAR)/$@; $(GORUN) .

check:
	go test -v ./...

build:
	go build -v ./...
