from itertools import product, combinations
import subprocess
import sys

# Amortized
suits = ['C', 'D', 'H', 'S']
ranks = [i for i in range(2, 10)]
for i in ['T', 'J', 'Q', 'K', 'A']:
	ranks.append(i)

cards = []
for suit in suits:
	for rank in ranks:
		cards.append(str(rank)+str(suit))

# Recompile before running
subprocess.run(["ghc", "-O2", "--make", "Proj1test"])
for target in combinations(cards, 2):
	command = ["./Proj1test"]
	for card in target:
		command.append(card)
	subprocess.run(command)