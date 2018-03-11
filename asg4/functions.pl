not( X ) :- X, !, fail.
not( _ ).

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961. 

deg_min_rads( degmin( Degrees, Minutes ), Rads ) :-
    Degs is Degrees + Minutes / 60,
    Rads is Degs * pi / 180.

total_hrs( time( Hours, Mins ), Totalhrs ) :-
    Totalhrs is Hours + Mins / 60.

hours_from_miles( Miles, Hours ) :-
    Hours is Miles / 500.

distance( Airport1, Airport2, Distance ) :-
   airport( Airport1, _, Lat1, Lon1 ),
   airport( Airport2, _, Lat2, Lon2 ),
   deg_min_rads( Lat1, Lat1_rads ),
   deg_min_rads( Lat2, Lat2_rads ),
   deg_min_rads( Lon1, Lon1_rads ),
   deg_min_rads( Lon2, Lon2_rads ),
   haversine_radians( Lat1_rads, Lon1_rads, Lat2_rads, Lon2_rads,
               Distance).

digsprint( Digits ) :-
    Digits < 10, print( 0 ), print( Digits ).

digsprint( Digits ) :-
    Digits >= 10, print( Digits ).

timeprint( Totalhrs ) :-
    Minsdigits is floor( Totalhrs * 60 ),
    Hours is Minsdigits // 60,
    Mins is Minsdigits mod 60,
    digsprint( Hours ), print( ':' ), digsprint( Mins ).

createflight( Terminal, Terminal, _, [Terminal], _ ).

createflight( Prev, Terminal, Visited, 
    [[Prev, FlightDep, FlightArr] | List], FlightDepInHM ) :-
    flight( Prev, Terminal, FlightDepInHM ),
    not( member( Terminal, Visited ) ),
    total_hrs( FlightDepInHM, FlightDep ),
    distance( Prev, Terminal, FDistance ),
    hours_from_miles( FDistance, TimeDiff ),
    FlightArr is FlightDep + TimeDiff,
    FlightArr < 24.0,
    createflight( Terminal, Terminal, [Terminal | Visited], List, _).

createflight( Prev, Terminal, Visited, 
    [[Prev, FlightDep, FlightArr] | List], FlightDepInHM ) :-
    flight( Prev, Next, FlightDepInHM ),
    not( member( Next, Visited ) ),
    total_hrs( FlightDepInHM, FlightDep ),
    distance( Prev, Next, FDistance ),
    hours_from_miles( FDistance, TimeDiff ),
    FlightArr is FlightDep + TimeDiff,
    FlightArr < 24.0,
    flight( Next, _, NextFlightDepInHM ),
    total_hrs( NextFlightDepInHM, NextFlightDep ),
    AdjTime is NextFlightDep - FlightArr - 0.5,
    AdjTime >= 0,
    createflight( Next, Terminal, [Next | Visited], 
        List, NextFlightDepInHM ).

writepath( [] ) :-
    nl.

writepath( [[X, XDTime, XATime], Y | []] ) :-
    airport( X, Depart_Ext, _, _), airport( Y, Arrive_Ext, _, _),
    write( 'depart  ' ),
    write( X ), write( '  ' ),
    write( Depart_Ext ), timeprint( XDTime ), nl,
    write( 'arrive  ' ),
    write( Y ), write( '  ' ),
    write( Arrive_Ext ), timeprint( XATime ), nl,
    !, true.

writepath( [[X, XDTime, XATime], [Y, YDTime, YATime] | Z] ) :-
    airport( X, Depart_Ext, _, _), airport( Y, Arrive_Ext, _, _),
    write( 'depart  ' ),
    write( X ), write( '  ' ),
    write( Depart_Ext ), timeprint( XDTime ), nl,
    write( 'arrive  ' ),
    write( Y ), write( '  ' ),
    write( Arrive_Ext ), timeprint( XATime ), nl,
    !, writepath( [[Y, YDTime, YATime] | Z] ).

fly( Depart, Depart ) :-
    write( 'Error: the departure and arrival of: ' ), write(Depart),
    write( ' to '), write(Depart), write( ' are the same.' ),
    nl,
    !, fail.

fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),

    createflight( Depart, Arrive, [Depart], List, _ ),
    !, nl,
    writepath( List ),
    true.

fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    write( 'Error: flight from: ' ), write(Depart),
    write( ' to '), write(Arrive), write( ' is not possible.' ),
    !, fail.

fly( _, _) :-
    write( 'Error: nonexistent airports.' ), nl,
!, fail.
