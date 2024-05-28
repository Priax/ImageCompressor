##
## EPITECH PROJECT, 2024
## B-PDG-300-MAR-3-1-PDGD02-vincent.montero-fontaine
## File description:
## Makefile
##

SRC = Main.hs

NAME = imageCompressor

OBJ = $(NAME).o $(NAME).hi

all: $(NAME)

$(NAME):
	stack build
	cp $(shell stack path --local-install-root)/bin/imageCompressor-exe imageCompressor

$(OBJ):
	stack ghc -- -c ./app/$(SRC)

clean:
	stack clean
	rm -f $(NAME)
	@find . -type f \( -name "*.hi" -o -name "*.o" \) -delete

fclean: clean
	find . -type f \( -name "*~" -o \( -name "*#" -a -name "#*" \) \) -delete
	find . -type f \( -name "*.hi" -o -name "*.o" \) -delete
	rm -f coding-style-reports.log

style: fclean
	lambdananas .

re:	fclean all

.PHONY: clean fclean style re
