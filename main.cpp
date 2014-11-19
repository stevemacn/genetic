/********************Yi Zhuo******************************
	Utils for the Genetic Algorithms
	**************************************************/

#include <iostream>
#include <string>
#include <stdlib.h>
using namespace std;

/**************************************Variables need to be predetermined********************************************************/

//initial population
#define POPULATION 10
//number of elements inside the function EX: COSX+4+5^2 had 7 elements MUST BE ODD SO THE LAST ELEMENTS IS ALWAY TERM
#define ELEMENTS 9
//number of fitness point we need to compare to determined the fitness level
#define CHECK_POINTS 10

//all the items that will be in the function to get graphs.
const string ope[] = {"+","-","*","/","^"};
const string term[] = {"cosx","sinx","x","0","1""2","3","4","5","6","7","8","9",".1"".2",".3",".4",".5",".6",".7",".8",".9"};

//populating the fitness array from the graph we are comparing to fitness_map[pos][value]
double fitness_map[2][CHECK_POINTS];


/********************************************************Things need to keep track of**********************************************/
double fitness_level[POPULATION]={0};
double total_fintess=0;
//double array for the initial population
string population[POPULATION][ELEMENTS];


/*************************************************************** FUNCTIONS********************************************************/


//populting the fitness_map
void populating_check(){
	//grap data from the graph we need to compare to
}

//random generator - take the size as input produce result vary from 0 to (size-1)
int random(int size){
	return rand()%size;
}

//populating the array population
void populating() {
	for( int x =0; x<POPULATION; ++x){
		for (int y =0; y<ELEMENTS; ++y){
			if( y%2==0){
				population[x][y]=term[random(sizeof(term)/sizeof(*term))];
			}
			else{
				population[x][y]=ope[random(sizeof(ope)/sizeof(*ope))];
			}
		}
	}
}

//roulette wheel
int roulette_wheel(){
	int selected;

	return selected;
}

//crossover
void crossover(int mom, int dad){

}

//mutation
void mutation(int victim){
	int mutation_rate=575; //some random number
	int randValue;
	for(int x=0; x<ELEMENTS; ++x){
		randValue=random(1000); //assignment random number to this variable
		if( randValue==mutation_rate){ //if it meet the mutation_rate then mutate
			if( x%2==0){
				population[victim][x]=term[random(sizeof(term)/sizeof(*term))];
			}
			else{
				population[victim][x]=ope[random(sizeof(ope)/sizeof(*ope))];
			}
		}
	}
}

//check fitness
void check_fitness(){
}

int main(){

	int mom, dad;
	populating();
	populating_check();

	//start
	mom=roulette_wheel();
	dad=roulette_wheel();
	crossover(mom,dad);
	mutation(mom);
	mutation(dad);
	check_fitness();

	//go back to the start
	getchar();
}