OPTIONS = -g4
demo: dfa.o utils.o nfa.o snapshots.o regex.o graph.o allen-nfa.o eventlogic.o demo.scm
	bigloo $(OPTIONS) dfa.o utils.o nfa.o snapshots.o regex.o graph.o allen-nfa.o eventlogic.o demo.scm -o demo
eventlogic: dfa.o utils.o nfa.o snapshots.o regex.o graph.o allen-nfa.o eventlogic.scm
	bigloo $(OPTIONS) dfa.o utils.o nfa.o snapshots.o regex.o graph.o allen-nfa.o eventlogic.scm -o eventlogic
allen-nfa: dfa.o utils.o nfa.o snapshots.o regex.o graph.o allen-nfa.scm 
	bigloo $(OPTIONS) dfa.o utils.o nfa.o snapshots.o regex.o graph.o allen-nfa.scm -o allen-nfa
# nfa: dfa.o utils.o nfa.scm
# 	bigloo $(OPTIONS)  nfa.scm dfa.o utils.o -o nfa
dfa: graph.o utils.o nfa.o regex.o dfa.scm
	bigloo $(OPTIONS) dfa.scm graph.o utils.o nfa.o regex.o -o dfa
# graph: dfa.o utils.o nfa.o graph.scm
# 	bigloo $(OPTIONS) graph.scm utils.o dfa.o nfa.o -o graph 
# regex: regex.scm dfa.o nfa.o utils.o 
# 	bigloo $(OPTIONS) regex.scm dfa.o nfa.o utils.o -o regex
snapshots: dfa.o utils.o nfa.o regex.o graph.o snapshots.scm
	bigloo $(OPTIONS) snapshots.scm dfa.o nfa.o utils.o regex.o graph.o -o snapshots
nfa.o:  nfa.scm
	bigloo $(OPTIONS) -c nfa.scm
dfa.o: dfa.scm
	bigloo $(OPTIONS) -c dfa.scm
utils.o: utils.scm
	bigloo $(OPTIONS) -c utils.scm
snapshots.o: snapshots.scm
	bigloo $(OPTIONS) -c snapshots.scm
graph.o: graph.scm
	bigloo $(OPTIONS) -c graph.scm
regex.o: regex.scm
	bigloo $(OPTIONS) -c regex.scm 
allen-nfa.o: allen-nfa.scm
	bigloo $(OPTIONS) -c allen-nfa.scm
eventlogic.o: eventlogic.scm
	bigloo $(OPTIONS) -c eventlogic.scm
clean:
	rm -f *.o
make:
	touch *.scm 
	touch *.sch
