default: epf-aula3.pdf

epf-aula3.pdf: 
	pandoc -t beamer Main.lhs -V theme:Luebeck -o epf-aula3.pdf

view: epf-aula3.pdf
	evince epf-aula3.pdf & 

clean:
	rm *.pdf
