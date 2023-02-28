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
	@echo 'Program purged'

re : fclean all

run : 
	@stack exec wolfram

tests_run: all
	@stack test

.PHONY: re run fclean clean all