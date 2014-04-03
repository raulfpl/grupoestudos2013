default: epf-aula4.pdf

epf-aula4.pdf: 
	pandoc -t beamer Main.lhs -V theme:Luebeck -o epf-aula4.pdf

view: epf-aula4.pdf
	evince epf-aula4.pdf & 

clean:
	rm *.pdf
