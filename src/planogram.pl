:- module(planogram,
    [ create_product_type/8,
    compute_facing_erp_id/1
]).

:- use_module(library('model/OWL')).

%% TODO : create planogram instance and assert describes the shop instance with id = store id
%% store contains shelf instances
%%
%%For now, using store id as 1.

create_planogram(StoreId) :-
    tell(instance_of(StorePlan, shop:'Planogram')),
    get_store_(Store, StoreId),
    tell(triple(StorePlan, dul:describes, Store)).

create_product_type(Name, Gtin, Dimension, Weight, Position, NumberOfFacing, ProductName, StoreId) :-
    [Depth, Width, Height] = Dimension,
    [ShelfId, ShelfLayerId, ProductOrder] = Position,
    tell([
    	is_class(ProductName),
    	subclass_of(ProductName, shop:'Product'),
    	is_class(AN),
    	subclass_of(AN, shop:'ArticleNumber'),
        instance_of(R, owl:'Restriction'),
    	is_restriction(R,value(shop:articleNumberOfProduct, AN)),
        subclass_of(ProductName, R),
    	triple(AN, shop:gtin, Gtin),

    	has_type(ProductShape, soma:'Shape'),
    	triple(ProductName, soma:hasShape, ProductShape),
    	object_dimensions(ProductName, Depth, Width, Height),

    	triple(ProductName, soma:hasMassValue, Weight),
        triple(ProductName, shop:hasOrder, ProductOrder),
        triple(ProductName, shop:numberOfFacing, NumberOfFacing),
    	has_label(ProductName, Name)]),
    
    get_shelf_(ShelfId, Shelf),
    get_shelf_layer_(ShelfLayerId, ShelfLayer),
    get_store_(Store, StoreId),

    tell([
        triple(Store, soma:contains, Shelf),
        triple(Shelf,soma:hasPhysicalComponent, ShelfLayer),
        has_type(Label, shop:'ShelfLabel'),
        triple(ShelfLayer, soma:hasPhysicalComponent, Label),
        triple(Label, shop:articleNumberOfLabel, AN)
        ]),
    
    numlist(1, NumberOfFacing, Num),
    forall(member(_, Num),
            tell([has_type(Facing, shop:'ProductFacingStanding'),
    	    triple(ProductName, dul:hasLocation, Facing),
            triple(Facing, shop:layerOfFacing, ShelfLayer)])).
    
compute_facing_erp_id(StoreId) :-
    forall((triple(Store, shop:hasShopId, StoreId),
        triple(Store, soma:contains, Shelf)),
        (has_type(Shelf, shop:'ShelfFrame'),
        get_layers_(Shelf))
    ).

get_layers_(Shelf) :-
    forall(triple(Shelf, soma:hasPhysicalComponent, Layer),
        (has_type(Layer, shop:'ShelfLayer'),
        get_order_and_assert_(Layer))
    ).

get_order_and_assert_(Layer):-
    findall([Order, Product],
        (triple(Layer, soma:hasPhysicalComponent, Label),
        writeln(Label),
        has_type(Label, shop:'ShelfLabel'),
        triple(Label, shop:articleNumberOfLabel, AN),
        writeln(AN), 
        is_restriction(ANDesc, value(shop:articleNumberOfProduct, AN)),
        instance_of(ANDesc, owl:'Restriction'),
        subclass_of(Product, ANDesc),!,
        writeln([ANDesc, Product]),
        subclass_of(Product, shop:'Product'),
        triple(Product, shop:hasOrder, Order),
        writeln(Order)),
        OrderAndProduct),
    
    sort(OrderAndProduct, SortedOrder),
    forall(member([O,P], SortedOrder),
        (create_facing_id_start_(O, P, FacingIdStart),
        writeln('All good'),
        triple(P, shop:numberOfFacing, NumberOfFacing),
        insert_facing_ids_(P,FacingIdStart, NumberOfFacing))).

insert_facing_ids_(P, IdStart, 1) :-
    triple(P, dul:hasLocation, Facing),
    tell(triple(Facing, shop:erpFacingId, IdStart)).

insert_facing_ids_(Product, IdStart, _) :-
    findall(Facing, 
        triple(Product, dul:hasLocation, Facing),
        Facings),
    tell_facing_id(Facings, IdStart).

tell_facing_id([F|Rest], Id) :-
    tell(triple(F,shop:erpFacingId, Id)),
    Id1 is Id+1,
    tell_facing_id(Rest, Id1).

tell_facing_id([], _).

create_facing_id_start_(Order, Product, FacingIdStart) :-
    \+ Order  is 1.0,
    numlist(1, Order, PriorOrders),
    findall(FacingNumber,
            (   
                member(ProductOrder, PriorOrders),
                triple(Product, shop:hasOrder, ProductOrder),
                triple(Product, shop:numberOfFacing, FacingNumber)
            ),
            FacingNumbers),
    sumlist(FacingNumbers, Temp),
    FacingIdStart is Temp+1.

create_facing_id_start_(_,_, 1).


get_shelf_(ShelfId, Shelf) :-
    triple(Shelf, shop:erpShelfId, ShelfId).

get_shelf_(ShelfId, Shelf) :-
    tell([ has_type(Shelf, shop:'ShelfFrame'),
            triple(Shelf, shop:erpShelfId, ShelfId)
        ]).

get_shelf_layer_(Id, Obj) :-
    triple(Obj, shop:erpShelfLayerId, Id).

get_shelf_layer_(Id, Obj) :-
    tell([ has_type(Obj, shop:'ShelfLayer'),
            triple(Obj, shop:erpShelfLayerId, Id)
        ]).

get_store_(Store, StoreId) :-
    triple(Store, shop:hasShopId, StoreId).

get_store_(Store, StoreId) :-
    tell([  has_type(Store, shop:'Shop'),
            triple(Store, shop:hasShopId, StoreId)
        ]).





:- begin_tests(planogram).

% test('product') :-
%     writeln('I am here'),
%     gtrace,
%     create_product_type('shampoo', 45344545, [0.8, 0.2, 0.1], 2.3, [17, 5, 1], 2, _, 1),
%     % create_product_type('soap', 453444563, [0.8, 0.2, 0.1], 2.3, [17, 5, 2], 3, _, 1),
%     compute_facing_erp_id(1).

test('recursion') :-
    gtrace,
    test_list([1,2,3,4]).

:- end_tests(planogram).