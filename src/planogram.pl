:- module(planogram,
    [ create_product_type/8
    % compute_facing_erp_id/1
]).

:- use_module(library('model/OWL')).

%%For now, using store id as 1.

create_planogram(StoreId) :-
    get_store_(Store, StoreId),
    tell([is_class(Plan),
        subclass_of(Plan, shop:'Planogram'),
        instance_of(R, owl:'Restriction'),
        has_description(R, exactly(soma:isDesignOf, 1, Store)),
        subclass_of(Plan, R)]).

create_product_type(Name, Gtin, Dimension, Weight, Position, NumberOfFacing, ProductName, StoreId) :-
    [Depth, Width, Height] = Dimension,
    [ShelfId, ShelfLayerId, ProductOrder] = Position,
    get_store_(Store, StoreId),
    tell([
    	is_class(ProductName),
    	subclass_of(ProductName, shop:'Product'),
        % instance of AN
    	instance_of(AN, shop:'ArticleNumber'),
        triple(AN, shop:gtin, Gtin),
        instance_of(R, owl:'Restriction'),
    	is_restriction(R,value(shop:articleNumberOfProduct, AN)),
        subclass_of(ProductName, R),

    	has_type(ProductShape, soma:'Shape'),
    	triple(ProductName, soma:hasShape, ProductShape),
    	object_dimensions(ProductName, Depth, Width, Height),

    	triple(ProductName, soma:hasMassValue, Weight),
        triple(ProductName, shop:hasOrder, ProductOrder),
        triple(ProductName, shop:numberOfFacing, NumberOfFacing),
    	has_label(ProductName, Name)]),
    
    get_shelf_(ShelfId, Store, Shelf),
    get_shelf_layer_(ShelfLayerId, Shelf, ShelfLayer),

    tell([
        % triple(Store, soma:contains, Shelf),
        % triple(Shelf,soma:hasPhysicalComponent, ShelfLayer),
        is_class(Label),
        subclass_of(Label, shop:'ShelfLabel'),
        is_restriction(R),
        is_restriction(R, value(shop:articleNumberOfLabel, AN)),
        subclass_of(Label, R),
        is_restriction(R1),
        is_restriction(R1, only(dul:isComponentOf, ShelfLayer)),
        subclass_of(Label, R1)
        ]),
    
    numlist(1, NumberOfFacing, Num),
    forall(member(_, Num),
            tell([ is_class(Facing),
            subclass_of(Facing, shop:'ProductFacingStanding'),
            is_restriction(R1),
            is_restriction(R1, only(dul:isLocationOf, ProductName)),
            subclass_of(Facing, R1),
            is_restriction(R2),
            is_restriction(R2, only(shop:layerOfFacing, ShelfLayer)),
            subclass_of(Facing, R2),
            is_restriction(R3),
            is_restriction(R3, only(shop:labelOfFacing, Label)),
            subclass_of(Facing, R3)
    	    % triple(ProductName, dul:hasLocation, Facing),
            % triple(Facing, shop:layerOfFacing, ShelfLayer)
            ])).
    
% compute_facing_erp_id(StoreId) :-
%     forall((triple(Store, shop:hasShopId, StoreId),
%         triple(Store, soma:contains, Shelf)),
%         (has_type(Shelf, shop:'ShelfFrame'),
%         get_layers_(Shelf))
%     ).

% get_layers_(Shelf) :-
%     forall(triple(Shelf, soma:hasPhysicalComponent, Layer),
%         (has_type(Layer, shop:'ShelfLayer'),
%         get_order_and_assert_(Layer))
%     ).

% get_order_and_assert_(Layer):-
%     findall([Order, Product],
%         (triple(Layer, soma:hasPhysicalComponent, Label),
%         writeln(Label),
%         has_type(Label, shop:'ShelfLabel'),
%         triple(Label, shop:articleNumberOfLabel, AN),
%         writeln(AN), 
%         is_restriction(ANDesc, value(shop:articleNumberOfProduct, AN)),
%         instance_of(ANDesc, owl:'Restriction'),
%         subclass_of(Product, ANDesc),!,
%         writeln([ANDesc, Product]),
%         subclass_of(Product, shop:'Product'),
%         triple(Product, shop:hasOrder, Order),
%         writeln(Order)),
%         OrderAndProduct),
    
%     sort(OrderAndProduct, SortedOrder),
%     forall(member([O,P], SortedOrder),
%         (create_facing_id_start_(O, P, FacingIdStart),
%         writeln('All good'),
%         triple(P, shop:numberOfFacing, NumberOfFacing),
%         insert_facing_ids_(P,FacingIdStart, NumberOfFacing))).

% insert_facing_ids_(P, IdStart, 1) :-
%     triple(P, dul:hasLocation, Facing),
%     tell(triple(Facing, shop:erpFacingId, IdStart)).

% insert_facing_ids_(Product, IdStart, _) :-
%     findall(Facing, 
%         triple(Product, dul:hasLocation, Facing),
%         Facings),
%     tell_facing_id(Facings, IdStart).

% tell_facing_id([F|Rest], Id) :-
%     tell(triple(F,shop:erpFacingId, Id)),
%     Id1 is Id+1,
%     tell_facing_id(Rest, Id1).

% tell_facing_id([], _).

% create_facing_id_start_(Order, Product, FacingIdStart) :-
%     \+ Order  is 1.0,
%     numlist(1, Order, PriorOrders),
%     findall(FacingNumber,
%             (   
%                 member(ProductOrder, PriorOrders),
%                 triple(Product, shop:hasOrder, ProductOrder),
%                 triple(Product, shop:numberOfFacing, FacingNumber)
%             ),
%             FacingNumbers),
%     sumlist(FacingNumbers, Temp),
%     FacingIdStart is Temp+1.

% create_facing_id_start_(_,_, 1).



get_shelf_(ShelfId, Store, ShelfFrame) :-
    is_restriction(R, value(shop:erpShelfId, ShelfId)), 
    subclass_of(ShelfFrame, R),
    subclass_of(ShelfFrame, shop:'ShelfFrame'),
    is_restriction(R1, only(soma:isContainedIn, Store)),
    subclass_of(ShelfFrame, R1),
    !.

get_shelf_(ShelfId, Store, ShelfFrame) :-
    tell([  is_class(ShelfFrame),
            subclass_of(ShelfFrame, shop:'ShelfFrame'),
            instance_of(R, owl:'Restriction'),
            is_restriction(R, value(shop:erpShelfId, ShelfId)),
            subclass_of(ShelfFrame, R),
            instance_of(R1, owl:'Restriction'),
            is_restriction(R1, only(soma:isContainedIn, Store)),
            subclass_of(ShelfFrame, R1)
        ]).

get_shelf_layer_(Id, Shelf, ShelfLayer) :-
    is_restriction(R1, only(dul:isComponentOf, Shelf)),
    subclass_of(ShelfLayer, R1),
    subclass_of(ShelfLayer, shop:'ShelfLayer'),
    is_restriction(R, value(shop:erpShelfLayerId, Id)),
    subclass_of(ShelfLayer, R),
    !.

get_shelf_layer_(Id, Shelf, ShelfLayer) :-
    tell([  is_class(ShelfLayer),
            subclass_of(ShelfLayer, shop:'ShelfLayer'),
            instance_of(R, owl:'Restriction'),
            is_restriction(R, value(shop:erpShelfLayerId, Id)),
            instance_of(R1, owl:'Restriction'),
            is_restriction(R1, only(shop:isComponentOf, Shelf)),
            subclass_of(ShelfLayer, R1),
            subclass_of(ShelfLayer, R)
        ]).

get_store_(Store, StoreId) :-
    subclass_of(Store, shop:'Shop'),
    is_restriction(RId, value(shop:hasShopId, StoreId)), 
    subclass_of(Store, RId),!.

get_store_(Store, StoreId) :-
    tell([  is_class(Store),
            subclass_of(Store, shop:'Shop'),
            instance_of(RId, owl:'Restriction'),
            is_restriction(RId, value(shop:hasShopId, StoreId)),
            subclass_of(Store, RId)
        ]).





%%%%%%%%%%%%%% Reasonign about differences

% 1. Do the shelves differ? 
% Check the number of vertices (Layers) if different yes else
% check the number of product types in each layer when differs yes
% 

%%% utils

compare_lists(A, B) :-
    is_list(A), 
    is_list(B), !.

compare_list(A, B) :-
    subsumes_term(A, B).

%% list comparison also works usign the following

elemcmp(A,B) :- var(A), var(B), ! ; A =@= B.

/* compare_list(A, B) :-
    maplist(elemcmp,A, B). */
    

    

:- begin_tests(planogram).

test('product') :-
    writeln('I am here'),
    gtrace,
    create_product_type('shampoo', 45344545, [0.8, 0.2, 0.1], 2.3, [17, 5, 1], 2, _, 1).
    % create_product_type('soap', 453444563, [0.8, 0.2, 0.1], 2.3, [17, 5, 2], 3, _, 1),
    % compute_facing_erp_id(1).

/* test('recursion') :-
    gtrace,
    compare_lists([1,2,3,4], [1,2,3,4]). */

:- end_tests(planogram).