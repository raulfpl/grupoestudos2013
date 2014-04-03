default: epf-aula5.pdf

epf-aula5.pdf: 
	pandoc -t beamer Main.lhs -V theme:Luebeck -o epf-aula5.pdf

view: clean epf-aula5.pdf
	evince epf-aula5.pdf & 

clean:
	rm *.pdf
	rm *.aux

