# hw05
#
# This Makefile is provided to help you organize the files
# for submission. It comes with ABSOLUTELY NO WARRANY and you
# are ultimately responsible for the files you submit.

.PHONY: all clean

# "I accidentally submitted old LaTeX..."  Don't be that person.
all: hw05-template.tex
	echo "Recompiling LaTeX..." ; pdflatex -jobname=hw05 hw05-template.tex ; pdflatex -jobname=hw05 hw05-template.tex 

clean:
	rm -f hw05.pdf
	rm -f hw05.aux 
	rm -f hw05.log
	rm -f hw05.out

