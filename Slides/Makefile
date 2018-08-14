SOURCES=$(shell find . -name *.Rmd)
SOURCES := $(wildcard *.Rmd)
TARGETS=$(SOURCES:%.Rmd=%.pdf)

%.pdf: %.Rmd
	@echo "$< -> $@"
	@Rscript -e "rmarkdown::render('$<')"

default: $(TARGETS)

deploy:
	rsync -zrvce 'ssh -p 18765' $(TARGETS) robjhynd@m80.siteground.biz:public_html/etc3580

clean:
	rm -rf $(TARGETS)