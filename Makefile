##
## EPITECH PROJECT, 2024
## MiniLibC
## File description:
## Makefile
##

BINARY_NAME = Wolfram-exe

NAME = wolfram

BINARY_PATH = $(shell stack path --local-install-root)

.PHONY:	all clean fclean re tests_run exec

all:	$(NAME)

$(NAME):
		stack build
		cp $(BINARY_PATH)/bin/$(BINARY_NAME) ./$(NAME)

clean:
		stack clean

fclean: clean
		rm -f $(NAME)

re:	fclean all

tests_run:
		stack test

exec:
		stack build
		clear
		stack exec $(BINARY_NAME)
