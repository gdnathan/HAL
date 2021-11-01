##
## EPITECH PROJECT, 2021
## makefile
## File description:
## HAL
##

MAKEFLAGS	+=	--no-print-directory
BINARY_PATH	:=	$(shell stack path --local-install-root)

all:
	stack build
	cp $(BINARY_PATH)/bin/hal-exe ./hal

clean:
	stack purge
	stack clean

fclean:	clean
	rm -f jal

tests_run:	clean
	stack test

re:	fclean all

.PHONY:	all clean fclean re debug test
