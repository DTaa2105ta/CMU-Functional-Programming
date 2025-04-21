# hw01
#
# This Makefile is provided to help you organize the files
# for submission. It comes with ABSOLUTELY NO WARRANY and you
# are ultimately responsible for the files you submit.

.PHONY: all clean

# "I accidentally submitted old LaTeX..."  Don't be that person.
all: hw01-template.tex
	echo "Recompiling LaTeX..." ; pdflatex -jobname=hw01 hw01-template.tex ; pdflatex -jobname=hw01 hw01-template.tex 

clean:
	rm -f hw01.pdf
	rm -f hw01.aux 
	rm -f hw01.log
	rm -f hw01.out

