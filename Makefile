GHC=$(shell command -v ghc 2>/dev/null || echo $(HOME)/.ghcup/bin/ghc)
PDFLATEX=$(shell command -v tectonic 2>/dev/null || command -v pdflatex 2>/dev/null || echo $(HOME)/.local/bin/tectonic)

.PHONY: build run example pdf report clean

build:
	$(GHC) --make Main.hs -o dka

run:
	./dka

example: build
	./dka "(a|b)*abb" -o example.tex

pdf: example
	$(PDFLATEX) example.tex

report:
	cd report && $(PDFLATEX) report.tex

clean:
	rm -f dka Main.o Main.hi Regex.o Regex.hi RegexParser.o RegexParser.hi Glushkov.o Glushkov.hi Latex.o Latex.hi
	rm -f example.tex example.log example.aux example.pdf dfa.tex
	rm -f report/report.aux report/report.log report/report.pdf
