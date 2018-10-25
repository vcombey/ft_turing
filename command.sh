clear && make && ./ft_turing -u && clear && ./ft_turing -e ressources/unary_add.json 1+11= | xargs ./ft_turing universal_turing_machine.json | ./ft_turing -d
