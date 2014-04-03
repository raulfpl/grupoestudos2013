default: epf-aula7.pdf

epf-aula7.pdf: 
	pandoc -t beamer -V theme:Luebeck Main.lhs -o epf-aula7.pdf
exec: 
	ghc --make Main.lhs -o Main
nasm-comp:
	nasm -f elf32 Expr.nasm
	ld -m elf_i386 Expr.o -o expr
clean:
	rm *.pdf
	rm *.o
	rm *.hi
	rm expr*
	rm *.nasm
	rm Main
