:- module(test,
    [ compare_lists/2
]).

:- use_module(library('reasoning/OWL/plowl/individual')
    [ owl_satisfied_by/3
]).

compare_lists(A, B) :-
    is_list(A), 
    is_list(B), !.

compare_list(A, B) :-
    subsumes_term(A, B).

%% list comparison also works usign the following

elemcmp(A,B) :- var(A), var(B), ! ; A =@= B.

/* compare_list(A, B) :-
    maplist(elemcmp,A, B). */

comp([_| R], Op, Y) :-
    % N is A *10,
    % (var(Op) -> Op is 0, X is N;
    X is Op + 1,
    comp(R, X, Y).

comp([], Op, Op). 

/* comp([], []). 

comp([], Y).

comp([A | R], Op) :-
    N is A *10,
    (var(Op) -> X is [N];
    append([N], Op, X)),
    comp(R, X).


len([],0).
len([_|T],N) :- len(T,X), N is X+1.

accLen([_|T],A,L) :- Anew is A+1, accLen(T,Anew,L).
accLen([],A,A). */

:- begin_tests(test).

/* test('comp') :-
    % gtrace,
    comp([1,2,3,4], 0, O1),
    writeln([0,O1]). */

test('owl_satisfied_by_value') :-
    tell([is_class(MyProductClass),
        subclass_of(MyProductClass, shop:'Product'),
        is_class(Facing),
        subclass_of(Facing, shop:'ProductFacingStanding'),
        is_restriction(R1),
        is_restriction(R1, only(dul:isLocationOf, MyProductClass)),
        subclass_of(Facing, R1)]),
    tell([ is_physical_object(FObj),
        is_physical_object(PObj),
        has_type(PObj, MyProductClass),
        has_type(FObj, shop:'ProductFacingStanding'),
        triple(FObj, dul:isLocationOf, PObj)
        ]),
    current_scope(QS),
    plowl_individual:owl_satisfied_by(R1, FObj, [QS,_{}]->FS).

test('cardinality cons') :-
    tell([is_class(Label),
        subclass_of(Label, shop:'ShelfLabel'),
        is_restriction(LabelFacingRest),
        is_restriction(LabelFacingRest,exactly(shop:facingAssociatedWithLabel, 2, shop:'ProductFacingStanding')),
        subclass_of(Label, LabelFacingRest)
        ]),
    
    tell([ is_physical_object(LabelIns),
        instance_of(LabelIns, shop:'ShelfLabel'),
        is_physical_object(FObj),
        is_physical_object(FObj1),
        has_type(FObj, shop:'ProductFacingStanding'),
        has_type(FObj1, shop:'ProductFacingStanding'),
        triple(LabelIns, shop:facingAssociatedWithLabel, FObj),
        triple(LabelIns, shop:facingAssociatedWithLabel, FObj1)
        ]),
        current_scope(QS),
        plowl_individual:owl_satisfied_by(LabelFacingRest, LabelIns, [QS,_{}]->FS).

assert_label_rel(LabelIns) :-
    triple(F, shop:labelOfFacing, LabelIns),
    tell(triple(LabelIns, shop:facingAssociatedWithLabel, F)),
    fail.

assert_label_rel(_).

is_value_restriction(R) :-
    triple(R, owl:hasValue, _).

is_article_value_equal_(V, V1) :-
    triple(V, shop:gtin, G),
    triple(V1, shop:gtin, G).

compare_component_value_(Class, Instance, Property) :-
    subclass_of(Class, R),
    is_restriction(R),
    has_description(R, value(Property, Val)),
    holds(Instance, Property, Val).

check_label_value_violation(value(Property, Value), Instane) :-
    holds(Instane, Property, Value1),
    is_article_value_equal(Value, Value1).

collect_restrictions(LabelC, LabelIns, Temp, Rest) :-
     % checks
    subclass_of(LabelC, Restriction),
    is_restriction(Restriction),
    \+ member(Restriction, Temp),
    ((is_value_restriction(Restriction),
    has_description(Restriction, Desc),
    check_label_value_violation(Desc, LabelIns),
    Temp1 = [Restriction | Temp]);
    (current_scope(QS),
    \+ plowl_individual:owl_satisfied_by(Restriction, LabelIns, [QS,_{}]->FS) -> 
    Temp1 = [Restriction  | Temp]; Temp1 = [])),
    collect_restrictions(LabelC, LabelIns, Temp1, Rest).

collect_restrictions(LabelC, LabelIns, Temp, Temp).

collect_restrictions(_, _, [], []) :- print_message(info, 'No violations').

get_violated_restrictions(LabelC, LabelIns, Rest) :-
    % Assert due to propertz discrepancies
    triple(LayerIns, soma:hasPhysicalComponent, LabelIns),
    has_type(LabelIns, shop:'ShelfLabel'),
    tell(triple(LabelIns, dul:isComponentOf, LayerIns)),
    assert_label_rel(LabelIns),
    collect_restrictions(LabelC, LabelIns, [], Rest).



test('check all the restrictions') :-
    tell([is_class(Layer),
        subclass_of(Layer, shop:'ShelfLayer'),
        is_class(Label),
        subclass_of(Label, shop:'ShelfLabel'),
        is_restriction(LabelFacingRest),
        is_restriction(LabelFacingRest,exactly(shop:facingAssociatedWithLabel, 2, shop:'ProductFacingStanding')),
        subclass_of(Label, LabelFacingRest),
        is_restriction(R1),
        is_restriction(R1, only(dul:isComponentOf, Layer)),
        subclass_of(Label, R1),
        is_restriction(R2),
        instance_of(AN, shop:'ArticleNumber'),
        triple(AN, shop:gtin, 6789),
        is_restriction(R2, value(shop:articleNumberOfLabel, AN)),
        subclass_of(Label, R2)
        ]),
    tell([is_class(Layer1),
        subclass_of(Layer1, shop:'ShelfLayer'), 
        is_physical_object(LabelIns),
        instance_of(LabelIns, shop:'ShelfLabel'),
        is_physical_object(FObj),
        is_physical_object(FObj1),
        has_type(FObj, shop:'ProductFacingStanding'),
        has_type(FObj1, shop:'ProductFacingStanding'),
        triple(FObj, shop:labelOfFacing, LabelIns),
        triple(FObj1, shop:labelOfFacing, LabelIns),
        instance_of(LayerIns, Layer1),
        triple(LayerIns, soma:hasPhysicalComponent, LabelIns),
        triple(LabelIns, shop:articleNumberOfLabel, AN)
        ]),
    
    gtrace,
    get_violated_restrictions(Label, LabelIns, Rest).       

    %% To check if 
    %% - Problem in the planogram represe to define the restriction I go from bottom 
    %% to top like e.g: label to layer.
    %% layer to shelf. But in realogram, it goes from top to bottom like layer to label,
    %% shelf to layer.

    

:- end_tests(test).