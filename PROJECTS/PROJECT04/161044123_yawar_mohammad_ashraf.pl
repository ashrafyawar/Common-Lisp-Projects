% Knowledge base
	
	flight(istanbul,izmir).
	flight(istanbul,antalya).
	flight(istanbul,gaziantep).
	flight(istanbul,ankara).
	flight(istanbul,van).
	flight(istanbul,rize).

	flight(edirne,edremit).

	flight(edremit,erzincan).
	flight(edremit,edirne).

	flight(izmir,isparta).
	flight(izmir,istanbul).

	flight(isparta,burdur).
	flight(isparta,izmir).

	flight(antalya,konya).
	flight(antalya, gaziantep).
	flight(antalya,istanbul).

	flight(konya,ankara).
	flight(konya,antalya).

	flight(ankara,van).
	flight(ankara,istanbul).
	flight(ankara,konya).

	flight(van,rize).
	flight(van,istanbul).
	flight(van,ankara).

	flight(gaziantep,istanbul).
	flight(gaziantep,antalya).

	flight(rize,istanbul).
	flight(rize,van).

	flight(erzincan,edremit).

	flight(burdur,isparta).
% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
% distance facts

	% NOTE : all the distances are in kilometer !!!

	distances(istanbul,izmir,328).
	distances(istanbul,antalya,482).
	distances(istanbul,gaziantep,847).
	distances(istanbul,ankara,351).
	distances(istanbul,van,1262).
	distances(istanbul,rize,967).
	
	distances(edremit,edirne,245).
	distances(edremit,erzincan,736).
	
	distances(erzincan,edremit,736).

	distances(edirne,edremit,245).

	distances(izmir,isparta,308).
	distances(izmir,istanbul,328).

	distances(isparta,izmir,308).
	distances(isparta,burdur,24).

	distances(burdur,isparta,24).

	distances(antalya,istanbul,482).
	distances(antalya,konya,192).
	distances(antalya, gaziantep,592).

	distances(konya,antalya,192).
	distances(konya,ankara,227).
	
	distances(gaziantep,antalya,592).
	distances(gaziantep,istanbul,847).

	distances(ankara,istanbul,351).
	distances(ankara,konya,227).
	distances(ankara,van,920).

	distances(van,ankara,920).
	distances(van,rize,373).
	distances(van,istanbul,1262).

	distances(rize,istanbul,967).
	distances(rize,van,373).
% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
% PART 1
	% rules

	% shows direct flights
	route(X,Y) :- flight(X,Y).
% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

% PART 2 

	% reffrence for this portion of code  <<<  https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15A.pl >>>

	related(X,Y,L) :- distances(X,Y,L).
	related(X,Y,L) :- distances(Y,X,L).

	path(A,B,Path,Len) :-
	       travel(A,B,[A],Q,Len), 
	       reverse(Q,Path).

	travel(A,B,P,[B|P],L) :- 
	       related(A,B,L).
	travel(A,B,Visited,Path,L) :-
	       related(A,C,D),           
	       C \== B,
	       \+member(C,Visited),
	       travel(C,B,[C|Visited],Path,L1),
	       L is D+L1.  

	sroute(A,B,Length) :-
	   setof([P,L],path(A,B,P,L),Set),
	   Set = [_|_], % fail if empty
	   minimal(Set,[Path,Length]).

	minimal([F|R],M) :- min(R,F,M).

	% minimal path
	min([],M,M).
	min([[P,L]|R],[_,M],Min) :- L < M, !, min(R,[P,L],Min). 
	min([_|R],M,Min) :- min(R,M,Min).
% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

% PART 3

	% when the each class will be given.
	when(102,10).
	when(108,12).
	when(341,14).
	when(455,16).
	when(452,17).

	%where the classes will be held at .
	where(102,z23).
	where(108,z11).
	where(341,z06).
	where(455,207).
	where(452,207).

	% who is gene take which class
	enrol(a,102).
	enrol(a,108).
	enrol(b,102).
	enrol(c,108).
	enrol(d,341).
	enrol(e,455).

	% solution for section 3.1
	schedule(STUDENT,PLACE,TIME):-
		where(CLASS_ID,PLACE), enrol(STUDENT,CLASS_ID),when(CLASS_ID,TIME).

	% solution for section 3.2
	usage(PLACE,TIME):-
		where(CLASS_ID,PLACE), when(CLASS_ID,TIME).
		
	% solution for section 3.3
	class_conflict(CLASS_A,CLASS_B):-
		where(CLASS_A,A), where(CLASS_B,B),not(A \= B).

	time_conflict(CLASS_A,CLASS_B):- 
		when(CLASS_A,A),when(CLASS_B,B),not(A \= B).
	
	conflict(CLASS_A,CLASS_B):-
		not(not(not((not(class_conflict(CLASS_A,CLASS_B)), not(time_conflict(CLASS_A,CLASS_B)))))).

	% solution for section 3.4
	meet(STUDENT_A,STUDENT_B):-
		enrol(STUDENT_A,CLASS_OF_A),where(CLASS_OF_A,PLACE_OF_A),enrol(STUDENT_B,CLASS_OF_B),where(CLASS_OF_B,PLACE_OF_B),CLASS_OF_A == CLASS_OF_B,PLACE_OF_A == PLACE_OF_B.

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

% PART 4 

	% solution for section 4.1
	element(X,[X|_]).
    element(X,[Y|T]) :- element(X,T).
% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>	