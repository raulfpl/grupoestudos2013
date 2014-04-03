default: epf-aula1.pdf

epf-aula1.pdf: 
	pandoc -t beamer Main.lhs -V theme:Luebeck -o epf-aula1.pdf

view: epf-aula1.pdf
	evince epf-aula1.pdf & 

clean:
	rm *.pdf
