default: epf-aula2.pdf

epf-aula2.pdf: 
	pandoc -t beamer Main.lhs -V theme:Luebeck -o epf-aula2.pdf

view: epf-aula2.pdf
	evince epf-aula2.pdf & 

clean:
	rm *.pdf
