##
## EPITECH PROJECT, 2021
## Makefile
## File description:
## It's my Makefile
##

NAME = wolfram

all :
	@stack build --copy-bins

clean:
	@stack clean
	@echo 'Program cleaned'

fclean:
	@stack purge
	@rm wolfram
	@echo 'Program purged'

re : fclean all

run : 
	@stack exec wolfram

tests_run:
	@stack clean
	@stack test --coverage

.PHONY: re run fclean clean all