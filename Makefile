NAME = ft_turing

all: 
	stack build
	cp ./.stack-work/install/x86_64-osx/lts-12.13/8.4.3/bin/ft-turing-exe $(NAME)

clean:
	rm $(NAME)
