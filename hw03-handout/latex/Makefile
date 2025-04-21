# hw03
#
# This Makefile is provided to help you organize the files
# for submission. It comes with ABSOLUTELY NO WARRANY and you
# are ultimately responsible for the files you submit.

.PHONY: all clean

# "I accidentally submitted old LaTeX..."  Don't be that person.
all: hw03-template.tex
	echo "Recompiling LaTeX..." ; pdflatex -jobname=hw03 hw03-template.tex ; pdflatex -jobname=hw03 hw03-template.tex 

clean:
	rm -f hw03.pdf
	rm -f hw03.aux 
	rm -f hw03.log
	rm -f hw03.out

