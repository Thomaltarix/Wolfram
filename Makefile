##
## EPITECH PROJECT, 2024
## MiniLibC
## File description:
## Makefile
##

BINARY_NAME = Wolfram-exe

NAME = Wolfram

BINARY_PATH = $(shell stack path --local-install-root)

.PHONY:	all clean fclean re

all:	$(NAME)

$(NAME):
		stack build
		cp $(BINARY_PATH)/bin/$(BINARY_NAME) ./$(NAME)

clean: stack clean

fclean: clean
		rm -f $(NAME)

re:	fclean all

e:
		stack build
		clear
		stack exec $(BINARY_NAME)