clear && make && ./ft_turing -u && clear && ./ft_turing -e $1 $2 | xargs ./ft_turing universal_turing_machine.json | ./ft_turing -d $1
