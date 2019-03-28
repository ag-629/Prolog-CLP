:-lib(lists).
:-lib(fd).
:-lib(fd_search).

guests(N):- 
    length(Gs, N), 
    makeList(Gs),
    print(Gs).

makeList([]).
makeList(Gs):-
    maleFemale(Gs,0,0),
    allAvailable(Gs),
    oneInteresting(Gs),
    oneFunny(Gs),
    friends(Gs),
    notFriends(Gs),
    political(Gs),
    alldifferent(Gs).

/*Each guest must be a male or a female*/
/*AND there must be equal numbers of men and women*/
maleFemale([],F,M):- F == M.
maleFemale([X|Others],F,M) :- 
    men(Y),member(X,Y), maleFemale(Others, F, M is M + 1);
    women(Y),member(X,Y), maleFemale(Others, F is F + 1, M).

/*There must be a day on which east guest is available*/
/*For each person'e availability on friday or saturday, it should be equal to a constraint variable*/
allAvailable(X) :- saturday(X, saturday); friday(X, friday).
    
saturday([], Z).
saturday([X|Others], Z) :- available(X, Z), saturday(Others, Z).

friday([], Z).
friday([X|Others], Z) :- available(X, Z), friday(Others, Z).

/*At least one interesting person is a guest*/
oneInteresting([X|Rest]) :- interesting(Y),member(X,Y).
oneInteresting([X|Rest]) :- oneInteresting(Rest).

/* At least one funny person is a guest*/
oneFunny([X|Rest]) :- funny(Y),member(X,Y).
oneFunny([X|Rest]) :- oneFunny(Rest).

/* Everyone knows someone else*/
friends(X) :- friendsFull(X, X). % Test if each person knows someone against the full list

friendsFull([X|Others], [X|Others]) :-
    friendsInd(X, [X|Others]);  % Check if a single person knows anyone else
    friendsFull(Others, [X|Others]). % Otherwise, check the rest of the list

/* Check individuals: Make sure the person knows someone*/
friendsInd(X, [Y|Others]) :- know(X, Y).
friendsInd(X, [Y|Others]) :- friendsInd(X, Others).

/*Everyone knows someone else*/
notFriends(X) :- not dislikeFull(X, X). % Test if each person knows someone against the full list

dislikeFull([X|Others], [X|Others]) :-
    dislikeInd(X, [X|Others]); % Check if a single person dislikes anyone else
    dislikeFull(Others, [X|Others]). % Otherwise, check the rest of the list

/* Check individuals: Make sure each person likes everyone else*/
dislikeInd(X, [Y|Others]) :- dislike(X, Y).
dislikeInd(X, [Y|Others]) :- dislikeInd(X, Others).

/* No mixing Republicans and Democrats*/
political(X) :- checkNotD(X); checkNotR(X).
    
checkNotD([]).
checkNotD([X|Others]) :- not democrat(X), checkNotD(Others).

checkNotR([]).
checkNotR([X|Others]) :- not republican(X), checkNotR(Others).


/* Data*/

men([tom, fred, billy, tim, frank, barry]).
women([sue, jane, betty, ellen, joan, betsy]).

funny([tom, sue, tim, ellen]).
interesting([fred, betty, frank, betsy]).

available(tom, saturday).
available(fred, saturday).
available(sue, saturday).
available(jane, saturday).
available(tim, saturday).  
available(frank, saturday).
available(ellen, saturday). 
available(joan, saturday).

available(tom, friday).
available(sue, friday).
available(tim, friday).
available(ellen, friday).
available(billy, friday).
available(betty, friday).
available(barry, friday).
available(betsy, friday).

democrat(tom).
democrat(sue).
democrat(tim).
democrat(ellen).

republican(fred).
republican(jane).
republican(frank).
republican(joan).

independent(billy).
independent(betty).
independent(barry).
independent(betsy).

know(tom, fred).
know(fred, tom).
know(fred, billy). 
know(billy, fred).
know(billy, betty). know(betty, billy).
know(betty, sue). know(tom, sue).
know(sue, tom). know(sue, betty).
know(sue, jane). know(jane, sue).
know(tim, frank). know(frank, tim).
know(frank, barry). know(barry, frank).
know(barry, betsy). know(betsy, barry).
know(betsy, ellen). know(tim, ellen).
know(ellen, tim). know(ellen, betsy).
know(ellen, joan). know(joan, ellen).

dislike(billy, fred).
dislike(sue, jane).
dislike(barry, frank).
dislike(ellen, joan).

/* Question 1: tea */
tea(Teas):-
    /* The amounts, in pounds of each tea */
    Teas = [T1,T2,T3], 
    
    /* Assign 0 through 20 to each type of tea */
    Teas :: [1..20],
    
    /* 570 is the cost of 20lbs at 2s 4.5d per pound */
    %the coefficients are the amount each tea costs in shillings
    30*T1 + 27 * T2 + 21 * T3 #= 570,
    
    /* The sum of the weights must be 20 lbs */
    T1 + T2 + T3 #= 20,
    
    print("\nThe first index is the amount, in pounds, of the best tea. The second is the second best tea, etc..."),
    
    labeling(Teas).

/*Question 2: Wine & Beer Barrels */    
beer(Barrels):-
    
    %there are 6 barrels
    Barrels = [B1, B2, B3, B4, B5, B6],
    
    %the possible values
    Barrels :: [15,16,18,19,20,31],
    
    
    B1 + B2 #= X,
    
    B3 + B4 + B5 #= 2 * X,
    
    %B6 is the barrel not used, the beer barrel
    % 119 is the total amount of gallons
    B6 #= 119 - 3 * X,
    
    alldifferent(Barrels),
    
    print("The First 2 indices represent the barrels sold to the first buyer.\n"),
    print("The follwong 3 represent the barrels sold to the second buyer.\n"),
    print("The values will switch around, but stay in the correct indices.\n"),
    printf("The beer barrel is the one containing %d gallons", B6),
    
    labeling(Barrels).


/* Question 3: Product using digits from 1 to 9 */

oneToNine(Values):-
    setupOneToNine(Values),
    solveOneToNine(Values),
    printOneToNine(Values).
    
setupOneToNine(Values):-
    
    Values = [X1, X2, X3, X4, X5, X6, X7, X8, X9],
    
    Values :: [1..9],
    
    alldifferent(Values),
    
    /*Find the matching product*/
    (100*X1 + 10* X2 + X3)*(10*X4 + X5) #= X,
    
    (10*X6 + X7)*(10*X8 + X9) #= X.
   
solveOneToNine(Values):-
    Values = [X1, X2, X3, X4, X5, X6, X7, X8, X9],
    alldifferent(Values),
    labeling(Values).

printOneToNine(Values):-
    
    Values = [X1, X2, X3, X4, X5, X6, X7, X8, X9],
    
    printf("The first value is: %d%d%d\n",[X1,X2,X3]),
    printf("The second value is: %d%d\n",[X4,X5]),
    printf("The third value is: %d%d\n",[X6,X7]),
    printf("The fourth value is: %d%d\n",[X8,X9]).


oneToNineMin(Values):-
    setupOneToNineMax(Values),
    solveOneToNineMax(Values),
    printOneToNineMax(Values).
    
setupOneToNineMax(Values):-
    
    Values = [X1, X2, X3, X4, X5, X6, X7, X8, X9],
    
    Values :: [1..9],
    
    alldifferent(Values),
    
    /*Find the matching product*/
    (100*X1 + 10* X2 + X3)*(10*X4 + X5) #= X,
    
    (10*X6 + X7)*(10*X8 + X9) #= X.
   
solveOneToNineMax(Values):-
    
    Values = [X1, X2, X3, X4, X5, X6, X7, X8, X9],
    alldifferent(Values),
    /*Multiply by -1 to find the maximum*/
    Product #= -1*(100*X1 + 10* X2 + X3)*(10*X4 + X5),
    minimize(labeling(Values),Product).

printOneToNineMax(Values):-
    
    Values = [X1, X2, X3, X4, X5, X6, X7, X8, X9],
    
    printf("The first value is: %d%d%d\n",[X1,X2,X3]),
    printf("The second value is: %d%d\n",[X4,X5]),
    printf("The third value is: %d%d\n",[X6,X7]),
    printf("The fourth value is: %d%d\n",[X8,X9]).
    

/* Question 4: Eggs */
eggs(Nums):-
    eggsSetup(Nums),
    eggsSolve(Nums),
    printEggs(Nums).

eggsSetup(Nums):-
/*Need a number n such that n mod [2,3,4,5,6] = 1 and n mod 7 = 0 */
    Nums = [W,X,Y,Z],
    /*Some high number*/
    Nums :: [1..10000],
    (4*W + 1) #= N,
    (5*X + 1) #= N,
    (6*Y + 1) #= N,
    (7*Z)     #= N.

eggsSolve(Nums):-
    Nums = [W,X,Y,Z],
    Val #= 4*W + 1,
    minimize(labeling(Nums),Val).

printEggs(Nums):-
    Nums = [W,X,Y,Z],
    Val #= 4*W +1,
    printf("The minimum value is %d",Val).

/*Question 5: Trusses of Hay */    
hay(Hays):-

    haySetup(Hays),
    haySolve(Hays),
    printHay(Hays).
    
haySetup(Hays):-
    Hays = [H1,H2,H3,H4,H5],
    /*If there are 10 pairs, each bail was weighed 4 times.*/
    /*By dividing the sum of each pair's weight by 4 */
    /*We get the sum of the individual weights*/
    Sum is div((110 + 112 + 113 + 114 + 115 + 116 + 117 + 118 + 120 + 121) , 4),
    
    /*There can't a bail that weighs more than 110*/
    /*Because it is the weight of the lightest pair*/
    Hays :: [1..110],
    
    
    /*The sum of bails*/
    H1 + H2 + H3 + H4 + H5 #= Sum,
    
    /*This fixes the value for the first bail*/
    /*The rest work from that*/
    H1 + H2 #= 110,
    H1 + H3 #>= 112,
    H1 + H4 #>= 113,
    H1 + H5 #>= 114.

haySolve(Hays):-
    Hays = [H1,H2,H3,H4,H5],
    labeling(Hays).
    
printHay(Hays):-
    Hays = [H1,H2,H3,H4,H5],
    printf("The weights are:\n%d lbs\n",H1),
    printf("%d lbs\n",H2),
    printf("%d lbs\n",H3),
    printf("%d lbs\n",H4),
    printf("and %d lbs",H5).
    
/*Question 6: Light Grid */
lightGrid(Grid):-
    lightGridSetup(Grid),
    lightGridSolve(Grid),
    printGrid(Grid).

lightGridSetup(Grid):-         
    
    Grid = [L1,L2,L3,L4,   
            L5,L6,L7,L8,
            L9,L10,L11,L12,
            L13,L14,L15,L16],
    Grid :: [0,1],
    
    /* Grid looks like this to start */
    /* W W W R */
    /* W W W R */
    /* R W R W */
    /* R W W W */
    
    /*These are the sums of the corresponding row and column for each red square*/
    /*The sum most be odd to ensure the color will change by the end*/
    L1 + L2 + L3 + L4 + L8 + L12 + L16 #= 2*A + 1,
    L5 + L6 + L7 + L8 + L4 + L12 + L16 #= 2*B + 1,
    L3 + L7 + L11 + L15 + L9 + L10 + L12 #= 2*C+1,
    L1 + L5 + L9 + L13 + L10 + L11 + L12 #= 2*D+1,
    L1 + L5 + L9 + L13 + L14 + L15 + L16 #= 2*E+1,
    
    /*These are the sums of the corresponding row and column for each white square*/
    /*The sum most be even to ensure the color will not change by the end*/
    L1 + L2 + L3 + L4 + L5 + L9 + L13 #= 2*F,
    L1 + L2 + L3 + L4 + L6 + L10 + L14 #= 2*G,
    L1 + L2 + L3 + L4 + L7 + L11 + L15 #= 2*H,
    L5 + L6 + L7 + L8 + L1 + L9 + L13 #= 2*I,
    L5 + L6 + L7 + L8 + L2 + L10 + L14 #= 2*J,
    L5 + L6 + L7 + L8 + L3 + L11 + L15 #= 2*K,
    L9 + L10 + L11 + L12 + L2 + L6 + L14 #= 2*M,
    L9 + L10 + L11 + L12 + L4 + L8 + L16 #= 2*N,
    L13 + L14 + L15 + L16 + L2 + L6 + L10 #= 2*O,
    L13 + L14 + L15 + L16 + L3 + L7 + L11 #= 2*P,
    L13 + L14 + L15 + L16 + L4 + L8 + L12 #= 2*Q.
    
    

lightGridSolve(Grid):-
    Grid = [L1,L2,L3,L4,
            L5,L6,L7,L8,
            L9,L10,L11,L12,
            L13,L14,L15,L16],
            
            labeling(Grid).

printGrid(Grid):-
    Grid = [L1,L2,L3,L4,   
            L5,L6,L7,L8,
            L9,L10,L11,L12,
            L13,L14,L15,L16],
    
    print("A one corresponds to that button being pressed\n"),
    printf("%d %d %d %d\n",[L1,L2,L3,L4]),
    printf("%d %d %d %d\n",[L5,L6,L7,L8]),
    printf("%d %d %d %d\n",[L9,L10,L11,L12]),
    printf("%d %d %d %d",[L13,L14,L15,L16]).
    
    
/*Question 7: Planets*/
planet(Letters):-
    planetSetup(Letters),
    planetSolve(Letters),
    printPlanet(Letters).

planetSetup(Letters):-
    %The unique letters
    Letters = [P,L,U,T,O,R,A,N,S,E,J,I,M,H,V,C,Y],
    
    Letters :: [1..18],
    
    P + L + U + T + O #= 40,
    U + R + A + N + U + S #= 36,
    S + A + T + U + R + N #= 33,
    J + U + P + I + T + E + R #= 50,
    M + A + R + S #= 32,
    E + A + R + T + H #= 31,
    M + O + O + N #= 36,
    V + E + N + U + S #= 39,
    M + E + R + C + U + R + Y #= 33,
    S + U + N #= 18,
    J #> I,
    C #> Y.
    
planetSolve(Letters):-
    Letters = [P,L,U,T,O,R,A,N,S,E,J,I,M,H,V,C,Y],
    alldifferent(Letters),
    labeling(Letters).

printPlanet(Letters):-
    Letters = [P,L,U,T,O,R,A,N,S,E,J,I,M,H,V,C,Y],
    Planets is P+L+A+N+E+T+S,
    printf("P = %d\n",P),
    printf("L = %d\n",L),
    printf("U = %d\n",U),
    printf("T = %d\n",T),
    printf("O = %d\n",O),
    printf("R = %d\n",R),
    printf("A = %d\n",A),
    printf("N = %d\n",N),
    printf("S = %d\n",S),
    printf("E = %d\n",E),
    printf("J = %d\n",J),
    printf("I = %d\n",I),
    printf("M = %d\n",M),
    printf("H = %d\n",H),
    printf("V = %d\n",V),
    printf("C = %d\n",C),
    printf("Y = %d\n",Y),
    printf("Planets = %d",Planets).

/*Question 8: Nephews' Wine */
wine(AllNephews):-

    AllNephews = [Nephew1,Nephew2,Nephew3,Nephew4,Nephew5],
    /*Lists of # of barrel types for each nephew*/
    /*the first index is the number of full barrels the second is the three quarter full barrels etc...*/
    Nephew1 = [N11,N12,N13,N14,N15],
    Nephew2 = [N21,N22,N23,N24,N25],
    Nephew3 = [N31,N32,N33,N34,N35],
    Nephew4 = [N41,N42,N43,N44,N45],
    Nephew5 = [N51,N52,N53,N54,N55],
    
    /*Each nephew must have at least one of each type of barrel*/
    Nephew1 :: [1..9],
    Nephew2 :: [1..9],
    Nephew3 :: [1..9],
    Nephew4 :: [1..9],
    Nephew5 :: [1..9],
    
    /*Each nephew should have nine barrels*/
    N11 + N21 + N31 + N41 + N51 #= 9,
    N12 + N22 + N32 + N42 + N52 #= 9,
    N13 + N23 + N33 + N43 + N53 #= 9,
    N14 + N24 + N34 + N44 + N54 #= 9,
    N15 + N25 + N35 + N45 + N55 #= 9,
    
    /*quantities scaled by 4 to avoid fractions*/
    /*each nephew should have 18 units of wine*/
    4*N11 + 3*N12 + 2*N13 + 1*N14 + 0*N15 #= 18,
    4*N21 + 3*N22 + 2*N23 + 1*N24 + 0*N25 #= 18,
    4*N31 + 3*N32 + 2*N33 + 1*N34 + 0*N35 #= 18,
    4*N41 + 3*N42 + 2*N43 + 1*N44 + 0*N45 #= 18,
    4*N51 + 3*N52 + 2*N53 + 1*N54 + 0*N55 #= 18,
    
    labeling(Nephew1),
    labeling(Nephew2),
    labeling(Nephew3),
    labeling(Nephew4),
    labeling(Nephew5),
    
    alldifferent(AllNephews),
    
    printWine(AllNephews).
    
printWine(AllNephews):-

    AllNephews = [Nephew1,Nephew2,Nephew3,Nephew4,Nephew5],
    
    Nephew1 = [N11,N12,N13,N14,N15],
    Nephew2 = [N21,N22,N23,N24,N25],
    Nephew3 = [N31,N32,N33,N34,N35],
    Nephew4 = [N41,N42,N43,N44,N45],
    Nephew5 = [N51,N52,N53,N54,N55],
    
    print("The numbers correspond to how many of each quantity the nephew gets.\n"),
    print("The first index is for the full barrels, the second for the three-quarters barrel...etc.\n"),
    
    printf("Nephew 1 gets: %d, %d, %d, %d, %d\n", [N11,N12,N13,N14,N15]),
    printf("Nephew 2 gets: %d, %d, %d, %d, %d\n", [N21,N22,N23,N24,N25]),
    printf("Nephew 3 gets: %d, %d, %d, %d, %d\n", [N31,N32,N33,N34,N35]),
    printf("Nephew 4 gets: %d, %d, %d, %d, %d\n", [N41,N42,N43,N44,N45]),
    printf("Nephew 5 gets: %d, %d, %d, %d, %d", [N51,N52,N53,N54,N55]).
    
/*Question 9: Party with constraints*/
guestsConstraint(N):-
    M is mod(N,2),
    /*must have even number of guests to ensure equal amounts of men and women*/
    M #= 0,
    length(Gs, N), 
    makeList(Gs),
    print(Gs).
