##
## EPITECH PROJECT, 2024
## B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
## File description:
## Makefile
##

NAME = imageCompressor

all: $(NAME)

# stack decides itself whether something must be rebuilt.
$(NAME):
	stack build
	cp $(shell stack path --local-install-root)/bin/$(NAME) $(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	find . -type f \( -name "*~" -o \( -name "*#" -a -name "#*" \) \) -delete
	rm -f coding-style-reports.log

style: fclean
	lambdananas .

re: fclean all

.PHONY: all $(NAME) clean fclean style re
