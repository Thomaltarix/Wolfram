##
## EPITECH PROJECT, 2024
## MiniLibC
## File description:
## Makefile
##

BINARY_NAME = Wolfram-exe

NAME = wolfram

BONUS_NAME = bonus_wolfram

BINARY_PATH = $(shell stack path --local-install-root)

.PHONY:	all clean fclean re tests_run exec bonus

all:	$(NAME)

$(NAME):
		stack build
		cp $(BINARY_PATH)/bin/$(BINARY_NAME) ./$(NAME)

clean:
		stack clean

fclean: clean
		rm -f $(NAME)
		rm -f $(BONUS_NAME)

re:	fclean all

tests_run:
		stack test

exec:
		stack build
		clear
		stack exec $(BINARY_NAME)

bonus:
		make fclean -C bonus/
		make -C bonus/
		cp bonus/$(BONUS_NAME) ./
