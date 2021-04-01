:- module(environment,
    [ 
        create_box/5,
        belief_marker_at/3,
        belief_shelf_at/2,
        belief_shelf_at/4
]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('model/SOMA/OBJ')).
:- use_module(library('ros/marker/marker_plugin')).

:- rdf_db:rdf_register_ns(soma, 
    'http://www.ease-crc.org/ont/SOMA.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(shop, 
    'http://knowrob.org/kb/shop.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(dmshop, 
    'http://knowrob.org/kb/dm-market.owl#', [keep(true)]).

%%%%% TODO : Michael -- Add a structure of Shelf. How a shelf should look like. Define it with a predicate. for eg, it has layers, two markers
%%%%% Belief marker at a pose then there is a shelf at ()

belief_marker_at(Marker, Id, Pose) :- %%% Left odd num and right even num ids
    tell(triple(Marker, dmshop:'markerId', Id)),
    (X is Id mod 2, X =:= 0 
    -> tell(has_type(Marker, dmshop:'DMShelfMarkerRight'))
    ; tell(has_type(Marker, dmshop:'DMShelfMarkerLeft'))),
    tell(is_at(Marker, Pose)),
    assert_shelf(Marker, Id, Pose).

belief_shelf_at(Shelf, Pose) :-
    tell(is_at(Shelf, Pose)),
    object_dimensions(Shelf, D, W, _),
    (\+ triple(Shelf, dmshop:leftMarker, _)
    ->  assert_left_shelf_marker(Shelf, Pose, D, W)),
    (\+ triple(Shelf, dmshop:rightMarker, _)
    ->  assert_right_shelf_marker(Shelf, Pose, D, W)).

belief_shelf_at(Marker, D, W, H) :-
    triple(Shelf, _, Marker),
    is_at(Shelf, ['map', [X, Y_old, Z_old], R]),
    is_at(Marker, ['map', [_, MY, _], _]),
    %retract_location_of(Shelf),
    Z is H/2,
    Y is MY-D,
    tell(is_at(Shelf, ['map', [X, Y, Z], R])),
    shop:retract_shape_of(Shelf),
    shop:retract_color_of(Shelf),
    assert_object_color__(Shelf, [0.7,0.2,1]),
    assert_object_shape__(Shelf, W, D, H).

create_box(D, W, H, Color, Obj) :-
    tell(is_physical_object(Obj)),
    assert_object_color__(Obj, Color),
    assert_object_shape__(Obj,D, W, H),
    triple(Shape,dul:hasRegion,ShapeRegion),
    triple(ColorType,dul:hasRegion,Region),

    Pos = [0,0,0], Rot = [0.0, 0.0, 0.0, 1],
    tell([is_individual(Origin),
        triple(ShapeRegion,'http://knowrob.org/kb/urdf.owl#hasOrigin',Origin),
        triple(Origin, soma:hasPositionVector, term(Pos)),
        triple(Origin, soma:hasOrientationVector, term(Rot)),
        triple(Region, soma:hasTransparencyValue, 1)
    ]).
    
assert_object_color__(Obj, Color) :-
    tell([has_type(ColorType, soma:'Color'),
    triple(Obj,soma:hasColor,ColorType),
    object_color_rgb(Obj, Color)]).

assert_object_shape__(Obj, D, W, H) :-
    tell([has_type(Shape, soma:'Shape'),
    holds(Obj,soma:hasShape,Shape),
    object_dimensions(Obj, D, W, H)]).


assert_left_shelf_marker(Shelf, Pose, Depth, Width) :-
    H = 0.1,
    [_, [Sx, Sy, _], _] = Pose,
    X is Sx + (Width/2),
    Y is Sy + Depth, 
    Z is H/2,
    create_box(0.1, 0.1, 0.1, [0.5, 0.5, 0.5], Marker),
    tell([has_type(Marker, dmshop:'DMShelfMarkerLeft'),
        triple(Shelf, dmshop:leftMarker, Marker),
        is_at(Marker, ['map', [X, Y, Z], [0,0,0,1]])]).

assert_right_shelf_marker(Shelf, Pose, Depth, Width) :-
    H = 0.1,
    [_, [Sx, Sy, _], _] = Pose,
    X is Sx - (Width/2),
    Y is Sy + Depth,
    Z is H/2,
    create_box(0.1, 0.1, 0.1, [0.5, 0.5, 0.5], Marker),
    tell([has_type(Marker, dmshop:'DMShelfMarkerRight'),
        triple(Shelf, dmshop:rightMarker, Marker),
        is_at(Marker, ['map', [X, Y, Z], [0,0,0,1]])]).

assert_shelf(Marker, Id, [_, [X,Y,Z], [X1,Y1,Z1,W1]]) :-
    (has_type(Marker, dmshop:'DMShelfMarkerRight')
    ->  ShelfId is Id/2,
        writeln(ShelfId),
    (triple(Shelf, shop:'ShelfId', ShelfId)
    ->  tell(triple(Shelf, dmshop:rightMarker, Marker)),
        adjust_shelf_pose(Shelf)
    ;   S_W = 1, S_D = 0.4, S_H = 1,
        create_box(S_D, S_W, S_H, [0.5, 0.5, 0.5], Shelf), %%% dim when not known   
        tell([triple(Shelf, dmshop:rightMarker, Marker),
            triple(Shelf, shop:'ShelfId', ShelfId)]),
        SX is X+(S_W/2), SZ is S_H/2,
        tell(is_at(Shelf, ['map', [SX, Y, SZ], [X1,Y1,Z1,W1]])))
    ; ShelfId is (Id+1)/2,
    (triple(Shelf, shop:'ShelfId', ShelfId)
    ->  tell(triple(Shelf, dmshop:leftMarker, Marker)),
        adjust_shelf_pose(Shelf)
    ;   S_W = 1, S_D = 0.4, S_H = 1,
        create_box(S_W, S_D, S_H, [0.5, 0.5, 0.5], Shelf), %%% dim when not known
        SX is X-(S_W/2), SZ is S_H/2, 
        tell([triple(Shelf, dmshop:leftMarker, Marker),
            triple(Shelf, shop:'ShelfId', ShelfId)]),
        tell(is_at(Shelf, ['map', [SX, Y, SZ], [X1,Y1,Z1,W1]])))
    ),
    writeln([Shelf,SX, Y, SZ]).

adjust_shelf_pose(Shelf) :-
    triple(Shelf, dmshop:leftMarker, LeftMarker),
    triple(Shelf, dmshop:rightMarker, RightMarker),

    is_at(LeftMarker, ['map', [LX, LY, _], _]),
    is_at(RightMarker, ['map', [RX, _, _], _]),

    shop:retract_shape_of(Shelf),
    SW is (LX - RX),
    assert_object_shape__(Shelf, SW, 0.4,1),
    SX is (LX - (SW /2)),

    is_at(Shelf, ['map', [_, _, SZ], _]),
    retract_location_of(Shelf),
    tell(is_at(Shelf, ['map', [SX, LY, SZ], [0,0,0,1]])),
    writeln(Shelf),
    writeln([SX, LY, SZ]).


retract_location_of(Object) :-
    triple(Object, soma:hasLocalization, Location),
    shop:retract_region_of(Location),
    shop:retract_entity(Location).

create_left_marker(Left, Pose) :-
    create_box(0.15,0.15,0.15, [1,1,0.2], Left),
    %Pose = ['map', [6.0, 1.2, 0.030], [0,0,0,1]],
    belief_marker_at(Left, 1, Pose).

create_right_marker(Right, Pose1) :-
    create_box(0.15,0.15,0.15, [1,1,0.2], Right),
    %Pose1 = ['map', [4.0, 1.2, 0.030], [0,0,0,1]],
    belief_marker_at(Right, 2, Pose1).


:- begin_tests(environment).

/* test('box create') :-
    gtrace,
    Pose = ['map', [6.0, 1.2, 0.030], [0,0,0,1]],
    create_left_marker(LM, Pose), % Box b color 0.7,0.2,1 pose - x+(d_b - d_a)/2, y with width, z is h/2  
    Pose1 = ['map', [5.0, 1.2, 0.030], [0,0,0,1]],
    create_right_marker(RM, Pose1),
    belief_shelf_at(LM, 0.5 , 1.0, 1.6),
    writeln([LM,RM]). */


test('belief shelf - assert markers') :-
    gtrace,
    create_box(1.0, 0.5, 1.6, [0.7,0.2,1], Shelf),
    PS = ['map', [8.2, 1.8, 0.8], [0,0,0,1]],
    belief_shelf_at(Shelf, PS),
    triple(Shelf, dmshop:leftMarker, L),
    triple(Shelf, dmshop:rightMarker, R),
    is_at(L, LP),
    is_at(R, RP),
    writeln([L, LP, R, RP]).

/* test('object color') :-
    % gtrace,
    tell(is_physical_object(O)),
    tell(has_type(ColorType, soma:'Color')), 
    tell(triple(O,soma:hasColor,ColorType)), 
    tell(object_color_rgb(O, [0.5,0.5,0.5])). */

:- end_tests(environment).
