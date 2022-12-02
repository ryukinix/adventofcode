GORUN = go run
YEAR := 2022

day%:
	@cd $(YEAR)/$@; $(GORUN) .
