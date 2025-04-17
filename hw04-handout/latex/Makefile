# hw04
#
# This Makefile is provided to help you organize the files
# for submission. It comes with ABSOLUTELY NO WARRANY and you
# are ultimately responsible for the files you submit.

.PHONY: all clean

# "I accidentally submitted old LaTeX..."  Don't be that person.
all: hw04-template.tex
	echo "Recompiling LaTeX..." ; pdflatex -jobname=hw04 hw04-template.tex ; pdflatex -jobname=hw04 hw04-template.tex 

clean:
	rm -f hw04.pdf
	rm -f hw04.aux 
	rm -f hw04.log
	rm -f hw04.out

