##
## EPITECH PROJECT, 2021
## HAL
## File description:
## Makefile
##

BINARY_PATH		:= $(shell stack path --local-install-root)

COVERAGE_PATH	:= $(shell stack path --local-hpc-root)

STACK_NAME		=	hal

NAME			=	hal

all:
	stack build
	cp $(BINARY_PATH)/bin/$(STACK_NAME)-exe ./$(NAME)
.PHONY:	all

debug:
	stack ghci
.PHONY:	debug

clean:
	stack clean
.PHONY:	clean

fclean: clean
	stack purge
	$(RM) $(NAME)
.PHONY:	fclean

re::	fclean
re::	all
.PHONY:	re

tests_run:
	stack test --coverage
.PHONY:	tests_run

coverage:
	firefox $(COVERAGE_PATH)/index.html
.PHONY:	coverage
