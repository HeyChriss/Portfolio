-module(tasks).
-export([start/0,generate_contestants/2]).



%% used to execute the simulated situation and we are using 20 contestants in the team 
%% I am using the shuffled 
start()->	
	Shuffled = list_lib:shuffle(generate_contestants([other,female,male],20)),
	list_lib:generate_teams(Shuffled,5).



%% Generates a defined number of contestants as if the data for them had been pulled from a database.

generate_contestants([],_) ->
	[];
generate_contestants([H|T],Count)->
	%% Since I don't have names from a database, I used the gender and a number for the name of the contestants.
	[{rand:uniform(),atom_to_list(H)++"_" ++ [N],H} || N<-lists:seq(1,Count)]++generate_contestants(T,Count).


	
