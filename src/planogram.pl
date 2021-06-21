:- module(planogram,
    [ 
    create_planogram/2,
    create_product_type/8,
    get_violated_restrictions/3
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

        % is_restriction(R1,value(shop:numberOfFacing, NumberOfFacing)),
        % subclass_of(ProductName, R1),
    	has_label(ProductName, Name)]),
    
    (get_shelf_(ShelfId, Store, Shelf); 
    tell_shelf_(ShelfId, Store, Shelf)),
    (get_shelf_layer_(ShelfLayerId, Shelf, ShelfLayer);
    tell_shelf_layer_(ShelfLayerId, Shelf, ShelfLayer)),

    tell([
        is_class(Label),
        subclass_of(Label, shop:'ShelfLabel'),
        is_restriction(R),
        is_restriction(R, value(shop:articleNumberOfLabel, AN)),
        subclass_of(Label, R),
        is_restriction(R1),
        is_restriction(R1, only(dul:isComponentOf, ShelfLayer)),
        subclass_of(Label, R1),
        %%% Add cardinality constraint on facing property to label
        is_restriction(LabelFacingRest),
        is_restriction(LabelFacingRest,exactly(shop:facingAssociatedWithLabel, NumberOfFacing, shop:'ProductFacingStanding')),
        subclass_of(Label, LabelFacingRest)
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
            ])).

% Compare between shelf and shelf instance - comments after disc with Sascha
% - If it exist - existence_of_shelf (both ways - real and plan shelf input)
% - existence_of_layers
% - return the restrictions that are not satisfied

shelf_individual_of(Shelf, ShelfClass, DiffRealogramLayers) :-
    get_equivalent_shelf_class_(Shelf, ShelfClass),

    %hack to fix the ids of the realogram layers as the top layers might not be scanned
    reorder_realogram_shelf_layer_numbers(ShelfClass, Shelf),

    %%% Check if the components satisfy 
   findall(Diff,  
        (triple(Shelf, soma:hasPhysicalComponent, LayerInstance),
        layer_instance_of(LayerInstance, ShelfClass, Diff)),
        DiffRealogramLayers).
        % length(LayerClasses, NumberOfPlannedShelfComponents),
        % length(Differences, NumberOfShelfComponents),
        % DiffRealogramLayers is NumberOfPlannedShelfComponents - NumberOfShelfComponents.


layer_instance_of(LayerInstance, ShelfClass, LayersWithProductDiff) :-
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

get_diff_in_facing_count(Id, Diff) :-
    triple(AN, shop:gtin, Id),
    get_number_of_facings_in_plan_(AN, PlannedNumberOfFacing),

    triple(Label, shop:articleNumberOfLabel, AN),
    aggregate_all(count, triple(_, 'http://knowrob.org/kb/shop.owl#labelOfFacing', Label) ,
    RealNoOfFacing),
    Diff is RealNoOfFacing - PlannedNumberOfFacing.

get_violated_restrictions(LabelClass, LabelInstance, Restrictions) :-
    % Assert due to property discrepancies
    triple(LayerIns, soma:hasPhysicalComponent, LabelInstance),
    has_type(LabelInstance, shop:'ShelfLabel'),
    tell(triple(LabelInstance, dul:isComponentOf, LayerIns)),
    
    % Check if the layer instance of label individual 
    % is an instance of the layer class
    subclass_of(R1, LabelClass),
    is_restriction(R1, only(dul:isComponentOf, LayerClass)),
    subclass_of(IdRest, LayerClass),
    is_restriction(IdRest, value(shop:erpShelfLayerId,_)),
    (owl_restriction_satisfied_by_(IdRest, LayerIns) ->
    tell(instance_of(LayerIns, LayerClass));
    true),

    assert_label_rel(LabelInstance),
    get_all_rest(LabelClass, AllRest),
    collect_restrictions(AllRest, LabelInstance, [], Restrictions).

assert_label_rel(LabelIns) :-
    triple(F, shop:labelOfFacing, LabelIns),
    tell(triple(LabelIns, shop:facingAssociatedWithLabel, F)),
    fail.

assert_label_rel(_).

get_all_rest(LabelC, Rest) :-
    findall(R, 
        (subclass_of(LabelC, R),
        is_restriction(R)),
    Rest).

collect_restrictions([X | R], LabelIns, Temp, Rest) :-
   (\+ owl_restriction_satisfied_by_(X, LabelIns),
   Temp1 = [X  | Temp]; Temp1 = Temp),
   collect_restrictions(R, LabelIns, Temp1, Rest).

collect_restrictions([], _, Temp, Temp).

collect_restrictions(_, _, [], []) :- print_message(info, 'No violations').

owl_restriction_satisfied_by_(X, Instance) :-
    is_value_restriction(X),
    has_description(X, Desc),
    check_label_value_violation(Desc, Instance).

owl_restriction_satisfied_by_(X, Instance) :-
    current_scope(QS),
    \+ plowl_individual:owl_satisfied_by(X, Instance, [QS,_{}]->FS).

check_label_value_violation(value(Property, Value), Instane) :-
    holds(Instane, Property, Value1),
    is_article_value_equal_(Value, Value1).

is_value_restriction(R) :-
    triple(R, owl:hasValue, _).

is_article_value_equal_(V, V1) :-
    triple(V, shop:gtin, G),
    triple(V1, shop:gtin, G).


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

get_equivalent_shelf_class_(_, ShelfClass) :-
    \+ var(ShelfClass).

get_equivalent_shelf_class_(Shelf, ShelfClass) :-
    triple(Shelf, shop:erpShelfId, Id),
    triple(Store, soma:containsObject, Shelf),
    triple(Store, shop:hasShopId, ShopId),

    is_restriction(RId, value(shop:hasShopId, ShopId)),
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
        _), RealNoOfLayers),
    
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
                triple(Facing, shop:productLabelOfFacing, P),
                triple(Facing, shop:layerOfFacing, Layer)
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
    % gtrace,
    create_realo_store(100, RealStore),
    create_realogram_test_shelf(RealStore, 17, 5, 45344545, 2),
    create_realogram_test_shelf(RealStore, 17, 5, 453444563, 3),
    get_shelfR(RealStore, 17, RealShelf),
    
    create_planogram(100, Store),
    create_product_type('shampoo', 45344545, [0.8, 0.2, 0.1], 2.3, [17, 5, 1], 2, 100, _),
    create_product_type('soap1', 453444563, [0.8, 0.2, 0.1], 2.3, [17, 5, 2], 3, 100 , _),

    get_shelf_(17, Store, ShelfClass),
    shelf_individual_of(RealShelf, ShelfClass, _).
    /*create_product_type('soap2', 45347565563, [0.8, 0.2, 0.1], 2.3, [17, 5, 3], 1, _, 100),
    create_product_type('soap3', 45354756563, [0.8, 0.2, 0.1], 2.3, [17, 5, 4], 1, _, 100). */
    % compute_facing_erp_id(1).

test('create realo') :-
    gtrace,
    create_realo_store(100, Store),
    create_realogram_test_shelf(Store, 17, 5, 45344545).
    
get_layers_test(Shelf, Temp, L1) :-
        triple(Shelf, soma:hasPhysicalComponent, L),
        writeln(L),
        get_layers_test(Shelf, [L| Temp],L1),
        fail.
    
get_layers_test(_, Temp, Temp).

get_layers_test(_, [], []) :- print_message(warning, 'no layers for the shelf').


test('without loop') :-
    tell([is_physical_object(Shelf),
    is_physical_object(Layer),
    is_physical_object(Layer1),
    is_physical_object(Layer2),
    triple(Shelf, soma:hasPhysicalComponent, Layer),
    triple(Shelf, soma:hasPhysicalComponent, Layer1),
    triple(Shelf, soma:hasPhysicalComponent, Layer2)]),
    get_layers_test(Shelf, [], L),
    triple(Shelf, soma:hasPhysicalComponent, L). 

:- end_tests(planogram).
