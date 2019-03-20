# Genetic-algorithms
==========================

A tool for optimization

* Generate a random population (each individual in the population has a parameter set)

* Calculate the fitness of each individual (according to some function)

* Reproduction with mutation and crossover


Reproduction
------------------------------------
* Each individual in the population is replaced at each generation

* Each replacement draws two parents with probability equal to their fitness

* The child of the parents contains a mix of the parent’s genes (one parent up to the randomly selected “crossover point”, and the other parent after that)

* Then, with some probability, the child experiences a random point mutation



Genetic algorithm for model selection
-------------------------------------
* Consider a bunch of variables that might be included in a linear model

* Our goal is to produce the best test R2 possible

* Code each “individual” as a binary series (e.g. 0001001110) where it is a 1 if the variable should be included in the model. Each 0 or 1 location is a “gene”

* The fitness of an individual is the test R2 


