OPTIONS = -g4
nfa: dfa.o utils.o nfa.scm
	bigloo $(OPTIONS)  nfa.scm dfa.o utils.o -o nfa
dfa: utils.o dfa.scm
	bigloo $(OPTIONS) dfa.scm utils.o -o dfa
nfa.o:  nfa.scm
	bigloo $(OPTIONS) -c nfa.scm
dfa.o: dfa.scm
	bigloo $(OPTIONS) -c dfa.scm
utils.o: utils.scm
	bigloo $(OPTIONS) -c utils.scm
graph: dfa.o utils.o nfa.o graph.scm
	bigloo $(OPTIONS) graph.scm utils.o dfa.o nfa.o -o graph 
situation: dfa.o utils.o nfa.o situation.scm
	bigloo $(OPTIONS) situation.scm dfa.o nfa.o utils.o -o situation
situation.o: situation.scm
	bigloo $(OPTIONS) -c situations.scm
regex: regex.scm dfa.o nfa.o utils.o 
	bigloo $(OPTIONS) regex.scm dfa.o nfa.o utils.o -o regex
regex.o: regex.scm
	bigloo $(OPTIONS) -c regex.scm 
