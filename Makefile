##
## EPITECH PROJECT, 2024
## MiniLibC
## File description:
## Makefile
##

BINARY_NAME = Wolfram-exe

NAME = wolfram

COVERAGE_NAME = Wolfram-test

BONUS_NAME = bonus_wolfram

BINARY_PATH = $(shell stack path --local-install-root)

COVERAGE_PATH = $(BINARY_PATH)/hpc/Wolfram/$(COVERAGE_NAME)/$(COVERAGE_NAME).tix

.PHONY:	all clean fclean re tests_run bonus

all:	$(NAME)

$(NAME):
		stack build
		cp $(BINARY_PATH)/bin/$(BINARY_NAME) ./$(NAME)

clean:
		stack clean
		make clean -sC bonus/

fclean: clean
		make fclean -sC bonus/
		rm -f $(NAME)
		rm -f $(BONUS_NAME)
		rm -rf test/coverage
		rm -f app/Main

re:	fclean all

tests_run:
		stack test --coverage
		mkdir -p test/coverage
		cp $(COVERAGE_PATH) test/coverage/

clean_tests:
		rm -rf test/coverage
		rm -f $(COVERAGE_PATH)
		rm -f app/Main

bonus:
		make re -sC bonus/
		cp bonus/$(BONUS_NAME) ./
