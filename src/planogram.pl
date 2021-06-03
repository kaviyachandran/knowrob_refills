:- module(planogram,
    [ create_product_type/8
    % compute_facing_erp_id/1
]).

:- use_module(library('model/OWL')).

%%For now, using store id as 1.

create_planogram(StoreId, Store) :-
    tell_store_(Store, StoreId),
    tell([is_class(Plan),
        subclass_of(Plan, shop:'Planogram'),
        instance_of(R, owl:'Restriction'),
        is_restriction(R, exactly(soma:isDesignOf, 1, Store)),
        subclass_of(Plan, R)]).

create_product_type(Name, Gtin, Dimension, Weight, Position, NumberOfFacing, StoreId, ProductName) :-
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

        is_restriction(R1,value(shop:numberOfFacing, NumberOfFacing)),
        subclass_of(ProductName, R1),
    	has_label(ProductName, Name)]),
    
    (get_shelf_(ShelfId, Store, Shelf); 
    tell_shelf_(ShelfId, Store, Shelf)),
    (get_shelf_layer_(ShelfLayerId, Shelf, ShelfLayer);
    tell_shelf_layer_(ShelfLayerId, Shelf, ShelfLayer)),

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

% Compare between shelf and shelf instance - comments after disc with Sascha
% - If it exist - existence_of_shelf (both ways - real and plan shelf input)
% - existence_of_layers
% - return the restrictions that are not satisfied

shelf_individual_of(Shelf, ShelfClass, DiffRealogramLayers) :-
    get_equivalent_shelf_class_(Shelf, ShelfClass),

    %hack to fix the ids of the realogram layers as the top layers might not be scanned
    reorder_realogram_shelf_layer_numbers(ShelfClass, Shelf),

   %%% Check if the components satisfy 
    %%% Check if the components satisfy 
   findall(Diff,  
        (triple(Shelf, soma:hasPhysicalComponent, LayerInstance),
        layer_instance_of(LayerInstance, ShelfClass, Diff)),
        Differences),
        length(LayerClasses, NumberOfPlannedShelfComponents),
        length(Differences, NumberOfShelfComponents),
        DiffInNumOfLayers is NumberOfPlannedShelfComponents - NumberOfShelfComponents.


layer_instance_of(LayerInstance, LayersWithProductDiff) :-
    triple(LayerInstance, shop:erpShelfLayerId, LayerId),
    get_shelf_layer_(LayerId, ShelfClass, LayerClass),
    get_products_in_layer_(LayerClass, ProductIds),

    findall(ProductDiff,
        (triple(LayerInstance, soma:hasPhysicalComponent, Label),
        has_type(Label, shop:'ShelfLabel'),
        triple(Label, shop:articleNumberOfLabel, AN),
        triple(AN, shop:gtin, InstanceId),
        (member(InstanceId, ProductIds) -> 
            get_diff_in_facing_count(InstanceId, Diff);
            Diff is -10),
        append([InstanceId], [Diff], ProductDiff)),
    ProductDiffs),
    append([LayerInstance], ProductDiffs, LayersWithProductDiff).

    %%% Check the parts of the shelf
    
get_diff_in_facing_count(Id, Diff) :-
    triple(AN, shop:gtin, Id),
    get_number_of_facings_in_plan_(AN, PlannedNumberOfFacing),

    triple(Label, shop:articleNumberOfLabel, AN),
    aggregate_all(count, triple(_, 'http://knowrob.org/kb/shop.owl#labelOfFacing', Label) ,
    RealNoOfFacing),
    Diff is RealNoOfFacing - PlannedNumberOfFacing.

%%%%%%%%%%%%%% Quantifying the differences

% 1. Do the shelves differ? 
% Check the number of vertices (Layers) if different yes else
% check the number of product types in each layer when differs yes
% 

%%%% 
% Computing Edit-distance considering vertex insertion, vertex deletion
% edge insertion, edge deletion
%%%%

compare_shelves(ShelfInPlan, RealShelf, Distance) :-
    get_all_shelf_layers_(ShelfInPlan, LayerClasses),

    aggregate_all(count, triple(RealShelf, 'http://www.ease-crc.org/ont/SOMA.owl#hasPhysicalComponent', 
        Layers), RealNoOfLayers),
    length(LayerClasses, NumberOfPlannedShelfComponents),
    DiffInNumOfLayers is LayerClasses - RealNoOfLayers,
    compute_layer_ids_(DiffInNumOfLayers, NumberOfPlannedShelfComponents, Ids),
    (DiffInNumOfLayers > 0 -> 
        call(insert_layer, ShelfInPlan, Ids, 0, Op);
        call(delete_layer, RealShelf, Ids, 0, Op)),
    
    %% Shelf layers to compare
    numlist(1, NumberOfPlannedShelfComponents, Idlist),
    subtract(Idlist, Ids, IdsToCompare),
    
    %%% Check if the components satisfy 
    member(Id, IdsToCompare),
    compare_shelf_layer(Id, ShelfInPlan, RealShelf, MoreOp).

compare_shelf_layer(Id, PlanShelf, RealShelf, Operations) :-
    get_shelf_layer_(Id, PlanShelf, LayerPlan).
    % Get the AN of label of the plan 
    % Get the AN of labels from the real layer
    % get the labels to be removed - subtract(RealLabel, PlanLabels, DeleteLabels)
    % Get the labels to be inserted - subtract(PlanLabel, RealLabels, InsertLabels)
    % Compare the facings of labels with same article number in both plan and real
    % intersection(PL, RL, CommonLabels).

%%%
% Insert labels and then facings  - 1 operation for each vertex and 
% edge insertion
%%%
insert_layer([Id | Rest], Shelf, Temp, Op) :-
    % inseriting a layer vertex + adding an edge
    Ins1 is Temp + 2,
    get_shelf_layer_(Id, Shelf, Layer),
    % get the number of labels
    aggregate_all(count, (triple(Restr, 'http://www.w3.org/2002/07/owl#onProperty','http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#isComponentOf'),
    triple(Restr, 'http://www.w3.org/2002/07/owl#allValuesFrom', Layer)), NumberOfLabels),
    % get the number of facings for each label
    findall(NoOfOp,
        (is_restriction(R1, only(dul:isComponentOf, LayerClass)),
        subclass_of(R1, Label),
        subclass_of(R, Label),
        is_restriction(R,value(shop:articleNumberOfLabel, AN)),
        triple(AN, shop:gtin, Id),
        is_restriction(R3,value(shop:articleNumberOfProduct, AN)),
        subclass_of(ProductName, R3),
        subclass_of(ProductName, R4),
        is_restriction(R4,value(shop:numberOfFacing, NumberOfFacing)),
        NoOfOp is 2*(1+NumberOfFacing)
        ),
    NoOfOps),
    sumlist(NoOfOps, Total),
    Ins2 is (2*NumberOfLabels) + Total + Ins1,
    insert_layer(Rest, Shelf, Ins2, Op).

insert_layer([], S, Temp, Temp).
    

%%
%  Get the number of labels, facings in the real shelf
%%
delete_layer([Id | Rest], Shelf, Temp, Op) :-
    Del1 is Temp +2,
    triple(Layer, shop:erpShelfLayerId, Id),
    triple(Shelf, soma:hasPhysicalComponent, Layer),

    findall(Op,
        (triple(Layer, soma:hasPhysicalComponent, Label),
        aggregate_all(count, triple(_, 'http://knowrob.org/kb/shop.owl#labelOfFacing', Label),
            NumberOfFacings),
        Op is 2*(1 + (2*NumberOfFacings))), % 1 - insertion of label 1 - for each facing and 1 - for each product in facing
        Ops),
    
    sumlist(Ops, Total),
    Del2 is Del1 + Total,
    delete_layer(Rest, Shelf, Del2, Op).

delete_layer([], S, Total, Total).

 /*    comp([_| R], Op, Y) :-
        % N is A *10,
        % (var(Op) -> Op is 0, X is N;
        X is Op + 1,
        comp(R, X, Y).
    
    comp([], Op, Op).  */

% util predicates

compute_layer_ids_(Num, NumPlan,Ids) :-
    Num > 0,
    numlist(Num, NumPlan, Ids);
    Start is Num * -1,
    numlist(Start, NumPlan, Ids).

get_number_of_facings_in_plan_(AN, Number):-
    is_restriction(R,value(shop:articleNumberOfProduct, AN)),
    subclass_of(_, R),
    subclass_of(_, R1),
    is_restriction(R1,value(shop:numberOfFacing, Number)).

get_all_shelf_layers_(Shelf, Ls) :-
    findall(L, 
        (is_restriction(R1, only(dul:isComponentOf, Shelf)),
        subclass_of(L, R1)), 
        Ls).

get_products_in_layer_(LayerClass, Ids) :-
    findall(Id,
        (is_restriction(R1, only(dul:isComponentOf, LayerClass)),
        subclass_of(R1, Label),
        subclass_of(R, Label),
        is_restriction(R,value(shop:articleNumberOfLabel, AN)),
        triple(AN, shop:gtin, Id)),
    Ids).

get_shelf_(ShelfId, Store, ShelfFrame) :-
    is_restriction(R, value(shop:erpShelfId, ShelfId)), 
    subclass_of(ShelfFrame, R),
    subclass_of(ShelfFrame, shop:'ShelfFrame'),
    is_restriction(R1, only(soma:isContainedIn, Store)),
    subclass_of(ShelfFrame, R1),
    !.

tell_shelf_(ShelfId, Store, ShelfFrame) :-
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

tell_shelf_layer_(Id, Shelf, ShelfLayer) :-
    tell([  is_class(ShelfLayer),
            subclass_of(ShelfLayer, shop:'ShelfLayer'),
            instance_of(R, owl:'Restriction'),
            is_restriction(R, value(shop:erpShelfLayerId, Id)),
            instance_of(R1, owl:'Restriction'),
            is_restriction(R1, only(dul:isComponentOf, Shelf)),
            subclass_of(ShelfLayer, R1),
            subclass_of(ShelfLayer, R)
        ]).

get_store_(Store, StoreId) :-
    is_restriction(RId, value(shop:hasShopId, StoreId)),
    subclass_of(Store, RId),
    subclass_of(Store, shop:'Shop').

tell_store_(Store, StoreId) :-
    tell([  is_class(Store),
            subclass_of(Store, shop:'Shop'),
            instance_of(RId, owl:'Restriction'),
            is_restriction(RId, value(shop:hasShopId, StoreId)),
            subclass_of(Store, RId)
        ]).

get_equivalent_shelf_class_(Shelf, ShelfClass) :-
    \+ var(ShelfClass).

get_equivalent_shelf_class_(Shelf, ShelfClass) :-
    triple(Shelf, shop:erpShelfId, Id),
    triple(Store, soma:containsObject, Shelf),
    triple(Store, shop:hasShopId, ShopId),

    is_restriction(RId, value(shop:hasShopId, StoreId)),
    subclass_of(Store, RId),
    subclass_of(Store, shop:'Shop'),
    
    is_restriction(R, value(shop:erpShelfId, Id)), 
    subclass_of(ShelfClass, R),
    is_restriction(R1, only(soma:isContainedIn, Store)),
    subclass_of(ShelfClass, R1).

%%% reorder the ids of the shelf layers before comparison

reorder_realogram_shelf_layer_numbers(ShelfClass,RealShelf) :-
    % get the number of shelf layers of a shelf in plan
    % get the number of layers in real
    aggregate_all(count, ((triple(Restr, 'http://www.w3.org/2002/07/owl#onProperty','http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#isComponentOf'),
    triple(Restr, 'http://www.w3.org/2002/07/owl#allValuesFrom', ShelfClass))), NumberOfPlannedShelfComponents),
    aggregate_all(count, triple(RealShelf, 'http://www.ease-crc.org/ont/SOMA.owl#hasPhysicalComponent', 
        Layers), RealNoOfLayers),
    
    Diff is NumberOfPlannedShelfComponents - RealNoOfLayers,
    % As the number of scanned layers are less than the real number of layers

    (Diff > 0 ->
    (forall(triple(RealShelf, 'http://www.ease-crc.org/ont/SOMA.owl#hasPhysicalComponent', Layer),
        (triple(Layer, shop:erpShelfLayerId, CurrId),
        CorrectedId is CurrId+Diff,
        tripledb_forget(Layer, shop:erpShelfLayerId, CurrId),
        tell(triple(Layer, shop:erpShelfLayerId, CorrectedId)))
    )); 
    true).


%%%%% Realo creation for testing 

create_realo_store(StoreNum, Store) :-
    tell([has_type(Store, shop:'Shop'),
        triple(Store, shop:hasShopId, StoreNum)]).

create_realogram_test_shelf(Store, ShelfNum, ShelfLayerNum, ProdId, NoOfFacings) :-
    get_shelfR(Store, ShelfNum, Shelf),
    get_shelflayerR(ShelfLayerNum, Shelf, Layer),

    tell([ instance_of(AN, shop:'ArticleNumber'),
        triple(AN, shop:gtin, ProdId),
        instance_of(R, owl:'Restriction'),
        is_restriction(R, value(shop:articleNumberOfProduct, AN)),
        is_class(P),
    	subclass_of(P, shop:'Product'),
        subclass_of(P, R),

        has_type(Label, shop:'ShelfLabel'),
        triple(Label,shop:articleNumberOfLabel, AN)
        ]),
    numlist(1, NoOfFacings, Num),
    forall(member(_, Num),
            tell([ has_type(Facing, shop:'ProductFacingStanding'),
                triple(Facing, shop:labelOfFacing, Label),
                triple(Facing, shop:productLabelOfFacing, P)
                ])).

get_shelfR(Store, Num, Shelf):-
    (triple(Shelf, shop:erpShelfId, Num),
    triple(Store, soma:containsObject, Shelf));
    tell([ has_type(Shelf, shop:'ShelfFrame'),
        triple(Shelf, shop:erpShelfId, Num),
        triple(Store, soma:containsObject, Shelf)]).

get_shelflayerR(Id, Shelf, L):-
    (triple(L, shop:erpShelfLayerId, Id),
    triple(Shelf, soma:hasPhysicalComponent, L));
    tell([ has_type(L, shop:'ShelfLayer'),
        triple(L, shop:erpShelfLayerId, Id),
        triple(Shelf, soma:hasPhysicalComponent, L)]).

get_product_and_art_num(ProdId, P, AN) :-
    triple(AN, shop:gtin, ProdId),
    is_restriction(R, value(shop:articleNumberOfProduct, AN)),
    subclass_of(P, R).

:- begin_tests(planogram).




test('product') :-
    writeln('I am here'),
    gtrace,
    create_realo_store(100, RealStore),
    create_realogram_test_shelf(RealStore, 17, 5, 45344545, 2),
    create_realogram_test_shelf(RealStore, 17, 5, 453444563, 3),
    get_shelfR(RealStore, 17, RealShelf),
    
    create_planogram(100, Store),
    create_product_type('shampoo', 45344545, [0.8, 0.2, 0.1], 2.3, [17, 5, 1], 2, 100, P1),
    create_product_type('soap1', 453444563, [0.8, 0.2, 0.1], 2.3, [17, 5, 2], 3, 100 , P2),

    get_shelf_(17, Store, ShelfClass),
    shelf_individual_of(RealShelf, ShelfClass, Diff).
    /*create_product_type('soap2', 45347565563, [0.8, 0.2, 0.1], 2.3, [17, 5, 3], 1, _, 100),
    create_product_type('soap3', 45354756563, [0.8, 0.2, 0.1], 2.3, [17, 5, 4], 1, _, 100). */
    % compute_facing_erp_id(1).

/* test('recursion') :-
    gtrace,
    compare_lists([1,2,3,4], [1,2,3,4]). */

    
get_layers_test(Shelf, Temp, L1) :-
        triple(Shelf, soma:hasPhysicalComponent, L),
        writeln(L),
        get_layers_test(Shelf, [L| Temp],L1),
        fail.
    
get_layers_test(Shelf, Temp, Temp).

get_layers_test(Shelf, [], []) :- print_message(warning, 'no layers for the shelf').


/* test('without loop') :-
    tell([is_physical_object(Shelf),
    is_physical_object(Layer),
    is_physical_object(Layer1),
    is_physical_object(Layer2),
    triple(Shelf, soma:hasPhysicalComponent, Layer),
    triple(Shelf, soma:hasPhysicalComponent, Layer1),
    triple(Shelf, soma:hasPhysicalComponent, Layer2)]),
    gtrace,
    get_layers_test(Shelf, [], L),
    % triple(Shelf, soma:hasPhysicalComponent, L),
    writeln(L). */

:- end_tests(planogram).
