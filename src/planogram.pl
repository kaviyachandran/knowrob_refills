:- module(planogram,
    [ create_product_type/6
]).

:- use_module(library('model/OWL')).

create_product_type(Name, Gtin, Dimension, Weight, Position, ProductName) :-
    [Depth, Width, Height] = Dimension,
    [ShelfId, ShelfLayerId, FacingId] = Position,
    tell([
    	is_class(ProductName)),
    	subclass_of(ProductName, shop:'Product'),
    	is_class(AN),
    	subclass_of(AN, shop:'ArticleNumber'),
    	is_restriction(R,value(shop:articleNumberOfProduct, AN)),
    	triple(AN, shop:gtin, Gtin),

    	has_type(ProductShape, soma:'Shape'),
    	triple(ProductName, soma:hasShape, ProductShape),
    	object_dimensions(ProductName, Depth, Width, Height),

    	triple(ProductName, soma:hasMassValue, Weight),
    	has_type(Facing, shop:'ProductFacingStanding'),
    	triple(ProductName, dul:hasLocation, Facing),

    	has_label(ProductName, Name)]),
    get_shelf_(ShelfId, Shelf),
    get_shelf_layer_(ShelfLayerId, ShelfLayer),  
    tell(triple(Facing, shop:layerOfFacing, ShelfLayer)).

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





:- begin_tests(planogram).

test('product') :-
    writeln('I am here'),
    gtrace,
    create_product_type('shampoo', 45344545, [0.8, 0.2, 0.1], 2.3, [17, 5, 2], P).

:- end_tests(planogram).