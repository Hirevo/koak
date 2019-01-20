NAME	=	koak

BPATH	=	$(shell stack path --local-install-root)/bin/$(NAME)

all:	$(NAME)

$(NAME):
	@stack build
	@cp $(BPATH) $(NAME)

clean:
	@stack clean

fclean:	clean
	@rm -rf $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
