# hw02
#
# This Makefile is provided to help you organize the files
# for submission. It comes with ABSOLUTELY NO WARRANY and you
# are ultimately responsible for the files you submit.

.PHONY: all clean

# "I accidentally submitted old LaTeX..."  Don't be that person.
all: hw02-template.tex
	echo "Recompiling LaTeX..." ; pdflatex -jobname=hw02 hw02-template.tex ; pdflatex -jobname=hw02 hw02-template.tex 

clean:
	rm -f hw02.pdf
	rm -f hw02.aux 
	rm -f hw02.log
	rm -f hw02.out

